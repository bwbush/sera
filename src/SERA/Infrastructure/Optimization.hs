{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Infrastructure.Optimization (
  NetworkGraph
, Vertex(..)
, Edge(..)
, networkGraph
, buildContext
, NetworkContext(..)
, EdgeContext(..)
, TechnologyContext(..)
, TechnologyBuilder
, Strategy(..)
, Optimum(..)
, optimize
) where


import Control.Arrow ((&&&), (***))
import Control.Monad (guard, when)
import Control.Monad.Log (logCritical, logDebug, logError, logInfo, logNotice)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (evaluate, knownKeys)
import Data.Daft.Vinyl.FieldCube (toKnownRecords, σ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Default.Util (inf)
import Data.Foldable (foldlM, toList)
import Data.Graph.Legacy.Algorithms (minimumCostFlow)
import Data.Graph.Legacy.Types (Graph(..), makeGraph)
import Data.List (find, nub, unzip4)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Monoid (Sum(..), (<>))
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tuple (swap)
import Data.Tuple.Util (snd3)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import SERA (SeraLog)
import SERA.Infrastructure.Flows
import SERA.Material (Pricer, localize, makePricer)
import SERA.Network (Network(..))
import SERA.Process.Reification (TechnologyOperation, operationReifier, technologyReifier)
import SERA.Process (ProcessLibrary(..), isProduction)
import SERA.Types.Cubes (DemandCube, IntensityCube, PeriodCube, PriceCube)
import SERA.Types.Fields (fArea, fCapacity, fCapitalCost, fCost, CostCategory(Salvage), fCostCategory, fDuration, fDutyCycle, fDelivery, fExtended, fFixedCost, fFrom, ImpactCategory(Consumption), fImpactCategory, Infrastructure(..), fInfrastructure, fLifetime, Material, fMaterial, fNameplate, fLength, Location, FLocation, fLocation, fSale, fSalvage, Pathway(..), fPathway, fPeriod, Productive(..), fProductive, fQuantity, fStage, Storage(..), fStorage, noStorage, Technology(..), fTechnology, fTo, fTransmission, fVariableCost, Year, fYear)
import SERA.Types.Records (Cash, Construction, Flow, Impact)

import qualified Data.Graph.Legacy.Types as G
import qualified Data.Map as M
import qualified Data.Sequence as Q
import qualified Data.Set as S
import qualified Numeric.LinearProgramming as L


(!) x y = case evaluate x y of
            Nothing -> error $ show y ++ " NOT FOUND IN " ++ show x
            Just z  -> z


modifyCapacities :: Bool -> VaryingFlows -> VaryingFlows
modifyCapacities True  = levelizeCapacities
modifyCapacities False = id


trace' = if False then trace else const id


prioritization :: Double -> Double
prioritization x = 1e12 - x -- FIXME: Use a tuple instead.


data Strategy =
    LiteralInWindow
  | SalvageAtWindowEnd
  | ExtrapolateBeyondWindow
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Strategy

instance ToJSON Strategy


data Optimum =
  Optimum
  {
    optimalConstruction :: [Construction]
  , optimalFlow         :: [Flow]
  , optimalCash         :: [Cash]
  , optimalImpact       :: [Impact]
  }
    deriving (Eq, Show)

instance Ord Optimum where
  compare x y =
    let
      capacity z = fNameplate <: z * fDutyCycle <: z
    in
      compare (sum . fmap capacity $ optimalConstruction y) (sum . fmap capacity $ optimalConstruction x)

instance Monoid Optimum where
  mempty      = Optimum
                {
                  optimalConstruction = []
                , optimalFlow         = []
                , optimalCash         = []
                , optimalImpact       = []
                }
  mappend x y = Optimum
                {
                  optimalConstruction = optimalConstruction x <> optimalConstruction y
                , optimalFlow         = optimalFlow         x <> optimalFlow         y
                , optimalCash         = optimalCash         x <> optimalCash         y
                , optimalImpact       = optimalImpact       x <> optimalImpact       y
                }

type NetworkGraph = Graph Vertex Edge


data Vertex =
    SuperSource
  | SuperSink
  | ConsumptionVertex Location
  | ProductionVertex Location
  | PathwayVertex Location Pathway Int
    deriving (Eq, Ord, Read, Show)


data Edge =
    DemandEdge Location
  | ExistingEdge Infrastructure
  | CentralEdge Location Technology
  | OnsiteEdge Location Technology
  | PathwayForwardEdge Location Pathway Int
  | PathwayReverseEdge Location Pathway Int
    deriving (Eq, Ord, Read, Show)


networkGraph :: Network -> DemandCube -> ProcessLibrary -> NetworkGraph
networkGraph Network{..} demandCube ProcessLibrary{..} =
  makeGraph
    $ concat
    [
      [
        (
          if stage == firstStage
            then ProductionVertex location
            else PathwayVertex location pathway stage'
        , if stage == lastStage
            then ConsumptionVertex location  -- FIXME: Only do this when there actually is consumption.
            else PathwayVertex location pathway $ stage' + 1
        , PathwayForwardEdge location pathway stage
        )
      |
        not (fExtended <: pathwayStage) || fDelivery <: pathwayStage
      , node <- toKnownRecords nodeCube
      , let location = fLocation <: node
      ]
      ++
      concat [
        [
          (from', to'  , PathwayForwardEdge location pathway stage) 
        , (to'  , from', PathwayReverseEdge location pathway stage) 
        ]
      |
        fExtended <: pathwayStage && fTransmission <: pathwayStage
      , link <- toKnownRecords linkCube
      , let location = fLocation <: link
            from = fFrom <: link
            to   = fTo   <: link
            from' = PathwayVertex from pathway stage'
            to'   = PathwayVertex to   pathway stage'
      ]
    |
      pathway <- S.toList $ S.map (fPathway <:) $ knownKeys pathwayCube
    , let stages = toKnownRecords $ σ (\key _ -> pathway == fPathway <: key) pathwayCube
          transmitting = fromMaybe maxBound $ (fStage <:) <$> find (\rec -> fExtended <: rec && fTransmission <: rec) stages
          mixed = isJust $ find (\rec -> fExtended <: rec && fTransmission <: rec && fDelivery <: rec) stages
          firstStage = fStage <: minimum stages
          lastStage = fStage <: maximum stages
    , pathwayStage <- stages
    , let stage = fStage <: pathwayStage
          stage' = if not mixed && stage <= transmitting then stage else stage - 1
    ]
    ++
    nub [
      (SuperSource, ProductionVertex $ fLocation <: existing, ExistingEdge $ fInfrastructure <: existing)
    |
      existing <- toKnownRecords existingCube
    ]
    ++
    nub
    [
      (ConsumptionVertex location, SuperSink, DemandEdge location)
    |
      demand <- toKnownRecords demandCube
    , let location = fLocation <: demand
    , (fLocation =: location) `S.member` knownKeys nodeCube
    ]
    ++
    nub
    [
      if productive == Onsite
        then (SuperSource, ConsumptionVertex  location, OnsiteEdge  location $ fTechnology <: production)
        else (SuperSource, ProductionVertex   location, CentralEdge location $ fTechnology <: production)
    |
      production <- toKnownRecords processCostCube
    , let productive = fProductive <: production
    , isProduction productive
    , node <- toKnownRecords nodeCube
    , let location = fLocation <: node
          productive' = fProductive <: node
    , productive == Central && productive' `elem` [Central, Yes] || productive == Onsite && productive' `elem` [Onsite, Yes]
    ]


type TechnologyBuilder = Int -> [Year] -> [Double] -> Maybe TechnologyContext


data TechnologyContext =
  TechnologyContext
  {
    construction :: Construction
  , operation    :: TechnologyOperation
  }


data EdgeContext =
    EdgeContext
    {
      builder    :: Maybe TechnologyBuilder
    , capacity   :: VaryingFlows
    , reserved   :: VaryingFlows
    , fixed      :: [TechnologyContext]
    , adjustable :: Maybe TechnologyContext
    , flows      :: [Flow]
    , cashes     :: [Cash]
    , impacts    :: [Impact]
    , reference  :: Maybe Double
    , debit     :: Double
    , pricer     :: Pricer
    }
  | EdgeReverseContext Edge


data NetworkContext =
  NetworkContext
  {
    timeContext  :: TimeContext
  , priceCube    :: PriceCube '[FLocation]
  , strategizing :: Strategy
  , discounting  :: Double
  , edgeContexts :: Map Edge EdgeContext
  }


makePricers :: PriceCube '[FLocation] -> Network -> Map Location Pricer
makePricers priceCube Network{..} =
  M.fromList
    [
      (fLocation <:location, makePricer priceCube location 0)
    |
      location <- S.toList (knownKeys nodeCube) ++ S.toList (knownKeys linkCube) 
    ]


remakePricers :: NetworkContext -> Map Location Pricer
remakePricers NetworkContext{..} =
  let
    feedstocks :: Map Location (Map (Material, Year) Double)
    feedstocks =
      M.fromListWith (M.unionWith (+))
        $
        [
          (fLocation <: rec, M.singleton (fMaterial <: rec, fYear <: rec) 0)
        |
          rec <- S.toList $ knownKeys priceCube
        , (fYear <: rec) `elem` yearz timeContext
        ]
        ++
        [
          (location, M.singleton (fMaterial <: impact, fYear <: impact) (- fQuantity <: impact))
        |
          (edge, edgeContext) <- M.toList edgeContexts
        , let (location, impacts'') = case edge of
                                        DemandEdge location'             -> (location', impacts edgeContext)
                                        ExistingEdge _                   -> (fLocation <: construction (head $ fixed edgeContext), impacts edgeContext)
                                        CentralEdge location' _          -> (location', impacts edgeContext)
                                        OnsiteEdge location' _           -> (location', impacts edgeContext)
                                        PathwayForwardEdge location' _ _ -> (location', impacts edgeContext)
                                        PathwayReverseEdge location' _ _ -> (location', [])
        , impact <- impacts''
        , fImpactCategory <: impact == Consumption
        ]
  in
    M.mapWithKey
      (
        \location consumptions material year ->
           (
             makePricer priceCube (fLocation =: location)
               $ M.findWithDefault 0 (material, year) consumptions
           )
            material year
      )
      feedstocks


reprice :: NetworkContext -> NetworkContext
reprice networkContext@NetworkContext{..} =
  let
    TimeContext{..} = timeContext
    pricers = remakePricers networkContext
  in
    networkContext
    {
      edgeContexts = M.mapWithKey
                       (
                         \edge edgeContext ->
                           let
                             (location, forward) = case edge of
                                                     DemandEdge location'             -> (location', True)
                                                     ExistingEdge _                   -> (fLocation <: construction (head $ fixed edgeContext), True)
                                                     CentralEdge location' _          -> (location', True)
                                                     OnsiteEdge location' _           -> (location', True)
                                                     PathwayForwardEdge location' _ _ -> (location', True)
                                                     PathwayReverseEdge location' _ _ -> (location', False)
                             edgeContext' = edgeContext { pricer = M.findWithDefault (error $ "No prices for \"" ++ show location ++ "\".") location pricers }
                             (flows', cashes', impacts') = costEdge' False strategizing yearz (zeroFlows timeContext) edgeContext
                           in
                             if forward
                               then edgeContext' { flows = flows' , cashes = cashes' , impacts = impacts' }
                               else edgeContext
                       )
                       edgeContexts
    }


buildContext :: NetworkGraph -> Network -> ProcessLibrary -> IntensityCube '[FLocation] -> PriceCube '[FLocation] -> DemandCube -> Double -> Strategy -> TimeContext -> NetworkContext
buildContext Graph{..} network@Network{..} processLibrary@ProcessLibrary{..} intensityCube priceCube demandCube discounting strategizing timeContext =
  let
    TimeContext{..} = timeContext
    pricers = makePricers priceCube network
    edgeContexts =
      M.fromList
        [
          (
            edge
          , (
              \edgeContext ->
                case edge of
                  DemandEdge{}         -> edgeContext { debit = prioritization . totalFlow . capacity $ edgeContext }
                  PathwayReverseEdge{} -> edgeContext
                  _                    -> let
                                            (flows', cashes', impacts') = costEdge' False strategizing yearz ({- FIXME: Checkt this. -} zeroFlows timeContext) edgeContext
                                          in
                                            edgeContext { flows = flows' , cashes = cashes' , impacts = impacts' }
            ) $
            case edge of
              DemandEdge location                       -> EdgeContext
                                                           {
                                                             builder    = Nothing
                                                           , capacity   = varyingFlows timeContext demandCube location
                                                           , reserved   = zeroFlows timeContext
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
                                                           , reference  = Nothing
                                                           , debit     = 0
                                                           , pricer     = pricers M.! location
                                                           }
              ExistingEdge infrastructure               -> let
                                                             existing = existingCube ! (fInfrastructure =: infrastructure <+> fPeriod =: head periods)
                                                           in
                                                             EdgeContext
                                                             {
                                                               builder    = Nothing
                                                             , capacity   = varyingFlows' timeContext existingCube infrastructure
                                                             , reserved   = zeroFlows timeContext
                                                             , fixed      = [
                                                                              let
                                                                                construction =     fInfrastructure =: infrastructure
                                                                                               <+> fLocation       =: fLocation <: existing
                                                                                               <+> fTechnology     =: fTechnology <: existing
                                                                                               <+> fProductive     =: Central
                                                                                               <+> fYear           =: fYear <: existing
                                                                                               <+> fLifetime       =: 1000
                                                                                               <+> fNameplate      =: sum
                                                                                                                        [
                                                                                                                          fCapacity <: (existingCube ! (fInfrastructure =: infrastructure <+> fPeriod =: period)) * duration
                                                                                                                        |
                                                                                                                          (period, duration) <- zip periods durations
                                                                                                                        ]
                                                                                               <+> fDutyCycle      =: 1
                                                                                               <+> fLength         =: 0
                                                                                               <+> fCapitalCost    =: 0
                                                                                               <+> fFixedCost      =: 0
                                                                                               <+> fVariableCost   =: fCost <: existing -- FIXME: Costs may vary.
                                                                                               <+> fStorage        =: noStorage
                                                                              in
                                                                                TechnologyContext
                                                                                {
                                                                                  construction = construction
                                                                                , operation    = operationReifier
                                                                                                   processLibrary
                                                                                                   (localize intensityCube $ fLocation <: existing)
                                                                                                   construction
                                                                                }
                                                                            ]
                                                             , adjustable = Nothing
                                                             , flows      = []
                                                             , cashes     = []
                                                             , impacts    = []
                                                             , reference  = Nothing
                                                             , debit     = 0
                                                             , pricer     = pricers M.! (fLocation <: existing)
                                                             }
              CentralEdge location technology           -> EdgeContext
                                                           {
                                                             builder    = Just
                                                                            $ \i year' demand ->
                                                                              uncurry TechnologyContext
                                                                                <$> technologyReifier
                                                                                      processLibrary
                                                                                      (localize intensityCube location)
                                                                                      (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show (head year') ++ "-" ++ show i) <+> fLocation =: location)
                                                                                      (minimum $ last year' : catMaybes (zipWith (\year'' demand'' -> guard (demand'' /= 0) >> return year'') year' demand))
                                                                                      (foldl mostExtreme 0 demand)
                                                                                      0
                                                                                      technology
                                                           , capacity   = zeroFlows timeContext
                                                           , reserved   = zeroFlows timeContext
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
                                                           , reference  = Nothing
                                                           , debit     = 0
                                                           , pricer     = pricers M.! location
                                                           }
              OnsiteEdge location technology            -> EdgeContext
                                                           {
                                                             builder    = Just
                                                                            $ \i year' demand ->
                                                                              uncurry TechnologyContext
                                                                                <$> technologyReifier
                                                                                      processLibrary
                                                                                      (localize intensityCube location)
                                                                                      (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show (head year') ++ "-" ++ show i) <+> fLocation =: location)
                                                                                      (minimum $ last year' : catMaybes (zipWith (\year'' demand'' -> guard (demand'' /= 0) >> return year'') year' demand))
                                                                                      (foldl mostExtreme 0 demand)
                                                                                      0
                                                                                      technology
                                                           , capacity   = zeroFlows timeContext
                                                           , reserved   = zeroFlows timeContext
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
                                                           , reference  = Nothing
                                                           , debit     = 0
                                                           , pricer     = pricers M.! location
                                                           }
              PathwayForwardEdge location pathway stage -> let
                                                             pathwayStage = pathwayCube ! (fPathway =: pathway <+> fStage =: stage)
                                                             technology = fTechnology <: pathwayStage
                                                             (_, distance') =
                                                               case linkCube `evaluate` (fLocation =: location) of
                                                                 Just link -> (fFrom <: link, fLength <: link)
                                                                 Nothing   -> (location, sqrt $ fArea <: (nodeCube ! (fLocation =: location)))
                                                             distance =
                                                               if fExtended <: pathwayStage
                                                                 then distance'
                                                                 else 0
                                                           in
                                                             EdgeContext
                                                             {
                                                               builder    = Just
                                                                              $ \i year' demand ->
                                                                                uncurry TechnologyContext
                                                                                  <$> technologyReifier
                                                                                        processLibrary
                                                                                        (localize intensityCube location)
                                                                                        (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show (head year') ++ "-" ++ show i) <+> fLocation =: location)
                                                                                        (minimum $ last year' : catMaybes (zipWith (\year'' demand'' -> guard (demand'' /= 0) >> return year'') year' demand))
                                                                                        (foldl mostExtreme 0 demand)
                                                                                        distance
                                                                                        technology
                                                             , capacity   = zeroFlows timeContext
                                                             , reserved   = zeroFlows timeContext
                                                             , fixed      = []
                                                             , adjustable = Nothing
                                                             , flows      = []
                                                             , cashes     = []
                                                             , impacts    = []
                                                             , reference  = Nothing
                                                             , debit     = 0
                                                             , pricer     = pricers M.! location
                                                             }
              PathwayReverseEdge location pathway stage -> EdgeReverseContext $ PathwayForwardEdge location pathway stage
          )
        |
          (identifier, edge) <- zip [(1::Int)..] $ S.toList allEdges
        ]
  in
    NetworkContext{..}


costEdge :: Strategy -> Double -> [Year] -> VaryingFlows -> EdgeContext -> Double
costEdge strategy discount year delta =
  sum
    . zipWith (*) [
                    if strategy == ExtrapolateBeyondWindow && n == length year - 1
                      then 1 / (1 + discount)**(fromIntegral n - 1) / discount
                      else 1 / (1 + discount)^n
                  |
                    n <- [0..] :: [Int]
                  ]
    . fmap (fSale <:)
    . snd3
    . costEdge' False strategy year delta -- FIXME: Handle salvage.


costEdge' :: Bool -> Strategy -> [Year] -> VaryingFlows -> EdgeContext -> ([Flow], [Cash], [Impact])
costEdge' isStorage strategy year delta EdgeContext{..} =
  let
    technologyContexts = maybe id (:) adjustable fixed
    (flows', cashes', impacts') = unzip3 . zipWith (\year' flow' -> costTechnologies isStorage pricer year' flow' technologyContexts) year . unvaryingFlows $ reserved .+. delta
  in
    (
      concat flows'
    , case strategy of
        SalvageAtWindowEnd -> concat cashes'
                              ++
                              [
                                fInfrastructure =: fInfrastructure <: rec <+> fYear =: fYear <: rec <+> fCostCategory =: Salvage <+> fSale =: (- fSalvage <: rec)
                              |
                                rec <- concat flows'
                              , fYear <: rec == last year
                              ]
        _                  -> concat cashes'
    , concat impacts'
    )
costEdge' _ _ _ _ _ = error "costEdge': edge is reversed."


costTechnologies :: Bool -> Pricer -> Year -> VaryingFlow -> [TechnologyContext] -> ([Flow], [Cash], [Impact])
costTechnologies isStorage pricer year flow technologyContexts =
  let
    (flows', cashes', impacts', _) = -- FIXME: check residue
      foldr (\t (flows'', cashes'', impacts'', residue'') -> let
                                                               (flows''', cashes''', impacts''', residue''') = costTechnology isStorage pricer year residue'' t
                                                             in
                                                               (flows''' ++ flows'', cashes''' ++ cashes'', impacts''' ++ impacts'', residue''')
            )
            ([], [], [], flow)
            technologyContexts
  in
    (flows', cashes', impacts')


costTechnology :: Bool -> Pricer -> Year -> VaryingFlow -> TechnologyContext -> ([Flow], [Cash], [Impact], VaryingFlow) -- TODO: Move into operations, which will return flow and residue.
costTechnology isStorage pricer year flow TechnologyContext{..} =                                            -- FIXME: Guard against overflow.
  let
    capacity' = fNameplate <: construction * fDutyCycle <: construction
    flow' =
      if isStorage || peakFlow flow <= capacity'
        then flow
        else signum flow #* capacity'
    residue = flow - flow'
    (flows', cashes', impacts', _) = (\ww@(w,_,_,_) -> trace'(show (flow, flow', residue, capacity', w)) ww) $ operation pricer year $ sumFlow flow'
  in
    if year < fYear <: construction
      then ([], [], [], flow)
      else ([flows'], cashes', impacts', residue)


rebuildEdgeContext :: [Year] -> VaryingFlows -> EdgeContext -> EdgeContext
rebuildEdgeContext years reserved' edgeContext@EdgeContext{..} =
  let
    flow = foldl mostExtreme 0 . fmap snd . unvaryingFlow <$> unvaryingFlows reserved'
    Just builder' = builder
    Just adjustable' = builder' (length fixed + 1) years flow
  in
    edgeContext
    {
      capacity = constantFlows' reserved' [
                   sum
                     [
                       if fYear <: fixed' <= year
                         then fNameplate <: fixed' * fDutyCycle <: fixed'
                         else 0
                     | fixed' <- construction <$> adjustable' : fixed
                     ]
                 |
                   year <- years
                 ]
    , reserved = reserved'
    , adjustable = Just adjustable'
    }


deltaCost :: Strategy -> Double -> [Year] -> VaryingFlows -> EdgeContext -> Double
deltaCost strategy discount years reserved' edgeContext@EdgeContext{..} =
   let
     oldCost = costEdge strategy discount years (zeroFlows' reserved ) {- $ rebuildEdgeContext years reserved  -} edgeContext
     newCost = costEdge strategy discount years (zeroFlows' reserved')    $ rebuildEdgeContext years reserved'    edgeContext
   in
--   newCost - oldCost / utilization reserved  -- FIXME: This was the utilization correction
     newCost - oldCost


marginalCost :: Bool -> Strategy -> Double -> [Year] -> VaryingFlows -> EdgeContext -> Double
marginalCost storageAvailable strategy discount year delta edgeContext =
  fromMaybe
    (
      let
        oldCost =                 costEdge strategy discount year (zeroFlows' delta)                                                     edgeContext
        newCost = fromMaybe inf $ costEdge strategy discount year (zeroFlows' delta) <$> adjustEdge storageAvailable strategy year delta edgeContext
      in
        if True
          then (newCost - oldCost) / sumAbs delta
          else newCost / sumAbs (reserved edgeContext .+. delta)
    )
    (reference edgeContext)


adjustEdge :: Bool -> Strategy -> [Year] -> VaryingFlows -> EdgeContext -> Maybe EdgeContext
adjustEdge storageAvailable strategy years delta edgeContext@EdgeContext{..} =
  do
    let
      reserved' = reserved .+. delta
      without = constantFlows' delta [
                  sum
                    [
                      if fYear <: fixed' <= year
                        then fNameplate <: fixed' * fDutyCycle <: fixed'
                        else 0
                    | fixed' <- construction <$> fixed
                    ]
                |
                  year <- years
                ]
    edgeContext' <-
      if reserved' #&<=# modifyCapacities storageAvailable capacity
        then return
               $ edgeContext
                 {
                   reserved = reserved'
                 }
        else do
               builder' <- builder
               adjustable' <-  -- FIXME: Check this.
                 builder'
                   (length fixed + 1)
                   years
                   $ signumDiff without reserved'
               return
                 $ edgeContext
                   {
                     capacity = constantFlows' delta [
                                  sum
                                    [
                                      if fYear <: fixed' <= year
                                        then fNameplate <: fixed' * fDutyCycle <: fixed'
                                        else 0
                                    | fixed' <- construction <$> adjustable' : fixed
                                    ]
                                |
                                  year <- years
                                ]
                   , reserved = reserved'
                   , adjustable = Just adjustable'
                   }
    let
      (flows', cashes', impacts') = costEdge' False strategy years (zeroFlows' delta) edgeContext'
    return
      $ edgeContext'
        {
          flows = flows'
        , cashes = cashes'
        , impacts = impacts'
        }
adjustEdge _ _ _ _ _ = error "adjustCapacity: edge is reversed."


data Capacity = Capacity VaryingFlows | Unlimited
  deriving (Show)

instance Eq Capacity where
  Unlimited  == Unlimited  = True
  Unlimited  == Capacity y = allInfiniteVaryingFlows y
  Capacity x == Unlimited  = allInfiniteVaryingFlows x
  Capacity x == Capacity y = minimumOfVaryingFlows y == minimumOfVaryingFlows x

instance Ord Capacity where
  Unlimited  `compare` Unlimited  = EQ
  Unlimited  `compare` Capacity y = if allInfiniteVaryingFlows y then EQ else LT
  Capacity x `compare` Unlimited  = if allInfiniteVaryingFlows x then EQ else GT
  Capacity x `compare` Capacity y = if True
                                      then minimumOfVaryingFlows y `compare` minimumOfVaryingFlows x
                                      else let
                                             x' = maximum (concat $ (snd <$>) . unvaryingFlow <$> unvaryingFlows x)
                                             y' = maximum (concat $ (snd <$>) . unvaryingFlow <$> unvaryingFlows y)
                                           in
                                             if x' == 0
                                               then GT
                                               else y' `compare` x'

instance Monoid Capacity where
  mempty = Unlimited
  Unlimited                 `mappend` Unlimited                 = Unlimited
  Unlimited                 `mappend` y                         = y
  x                         `mappend` Unlimited                 = x
  Capacity (VaryingFlows x) `mappend` Capacity (VaryingFlows y) = if False -- FIXME: Only for backwards compatibility testing.
                                                                    then if (concat $ (snd <$>) . unvaryingFlow <$> x) < (concat $ (snd <$>) . unvaryingFlow <$> y)
                                                                      then Capacity $ VaryingFlows x
                                                                      else Capacity $ VaryingFlows y
                                                                    else Capacity $ VaryingFlows
                                                                           [
                                                                             VaryingFlow
                                                                               [
                                                                                 (d, minimum [x'', y''])
                                                                               |
                                                                                 ((d, x''), (_, y'')) <- zip x' y'
                                                                               ]
                                                                           |
                                                                             (VaryingFlow x', VaryingFlow y') <- zip x y
                                                                           ]

positiveFlow :: VaryingFlows -> Bool
positiveFlow (VaryingFlows x) = or $ any ((> 0) . snd) . unvaryingFlow <$> x


costFunction :: Bool -> [Year] -> Capacity -> NetworkContext -> Edge -> Maybe (Sum Double, NetworkContext)
costFunction storageAvailable year (Capacity flow) context edge =
  (\x -> trace' ("COST\t" ++ show edge ++ "\t" ++ show (fst <$> x) ++ "\t" ++ show flow) x) $ do
    let
      edgeContext = edgeContexts context M.! edge
      (capacity', builder') =
        case edgeContext of
          EdgeContext{..}          -> (modifyCapacities storageAvailable capacity .-. (absFlows reserved), builder)
          EdgeReverseContext edge' -> (\z -> modifyCapacities storageAvailable (capacity z) .-. (absFlows $ reserved z)) &&& builder $ edgeContexts context M.! edge'
      canBuild =  canBuild' year flow builder'
    guard
      $ positiveFlow capacity' && compatibleFlow capacity' flow || canBuild
    case edge of
      DemandEdge _                              -> return (Sum $ debit edgeContext, context)
      ExistingEdge _                            -> if False -- FIXME: Pricing formerly was incomplete.
                                                     then return (Sum . sum $ (fVariableCost <:) . construction <$> fixed edgeContext, context)
                                                     else  return (Sum $ marginalCost storageAvailable (strategizing context) (discounting context) year flow edgeContext, context)
      PathwayReverseEdge location pathway stage -> costFunction storageAvailable year (Capacity (negate flow)) context $ PathwayForwardEdge location pathway stage
      _                                         -> return (Sum $ marginalCost storageAvailable (strategizing context) (discounting context) year flow edgeContext, context)
costFunction _ _ _ _ _ = Nothing -- error "costFunction: no flow."


capacityFunction :: Bool -> [Year] -> NetworkContext -> Edge -> Maybe (Capacity, NetworkContext)
capacityFunction storageAvailable year context edge =
  (\x -> trace' ("CAPACITY\t" ++ show edge ++ "\t" ++ show (fst <$> x)) x) $ case edgeContexts context M.! edge of
    EdgeContext{..}          -> do
                                  let
                                    canBuild =  canBuild' year (veryConstantFlows (timeContext context) 1) builder
                                  guard
                                    $ reserved #<# modifyCapacities storageAvailable capacity || canBuild
                                  return
                                    (
                                      if canBuild
                                        then Capacity $ veryConstantFlows (timeContext context) inf
                                        else Capacity $ modifyCapacities storageAvailable capacity .-. abs reserved
                                    , context
                                    )
    EdgeReverseContext edge' -> capacityFunction storageAvailable year context edge'


canBuild' :: [Year] -> VaryingFlows -> Maybe TechnologyBuilder -> Bool
canBuild' _    flow Nothing        = maximumAbs flow == 0
canBuild' year flow (Just builder) = isJust $ builder 0 year $ peakAnnualFlows flow


flowFunction :: Bool -> [Year] -> Capacity -> NetworkContext -> Edge -> NetworkContext
flowFunction storageAvailable year (Capacity flow) context edge =
  fromMaybe (trace'("FAILED TO SET FLOW\t" ++ show edge ++ "\t" ++ show (construction <$> adjustable (edgeContexts context M.! edge)) ++ "\t" ++ show (construction <$> fixed (edgeContexts context M.! edge)) ++ "\t" ++ show year ++ "\t" ++ show flow) context)
    $ do
        let
          edgeContext = edgeContexts context M.! edge
          update edgeContext' = context { edgeContexts = M.insert edge (edgeContext' { reference = Nothing } ) $ edgeContexts context }
          (capacity', builder') =
            case edgeContext of
              EdgeContext{..}          -> (modifyCapacities storageAvailable capacity .-. abs reserved, builder)
              EdgeReverseContext edge' -> (\z -> modifyCapacities storageAvailable (capacity z) .-. abs (reserved z)) &&& builder $ edgeContexts context M.! edge'
          canBuild =  canBuild' year flow builder'
        guard
          $ positiveFlow capacity' || canBuild
        case edge of
          DemandEdge _                              -> trace' ("FLOW\t" ++ show edge ++ "\t" ++ show flow) $ return . update $ edgeContext { reserved = reserved edgeContext .+. flow}
          ExistingEdge _                            -> trace' ("FLOW\t" ++ show edge ++ "\t" ++ show flow) $ return . update $ let
                                                                  (flows', cashes', impacts') = costEdge' False (strategizing context) year (zeroFlows $ timeContext context) $ edgeContext { reserved = reserved edgeContext .+. flow }
                                                                in
                                                                  edgeContext
                                                                  {
                                                                    reserved = reserved edgeContext .+. flow
                                                                  , flows    = flows'
                                                                  , cashes   = cashes'
                                                                  , impacts  = impacts'
                                                                  }
          PathwayReverseEdge location pathway stage -> return $ flowFunction storageAvailable year (Capacity (negate flow)) context $ PathwayForwardEdge location pathway stage
          _                                         -> trace' ("FLOW\t" ++ show edge ++ "\t" ++ show flow) $ update <$> adjustEdge storageAvailable (strategizing context) year flow edgeContext
flowFunction _ _ _ _ _ = error "flowFunction: no flow."


optimize :: SeraLog m => [[Year]] -> PeriodCube -> Network -> DemandCube -> IntensityCube '[FLocation] -> ProcessLibrary -> PriceCube '[FLocation] -> Double -> Double -> Strategy -> Bool -> Double -> m (Bool, Optimum)
optimize yearses periodCube network demandCube intensityCube processLibrary priceCube discountRate _escalationRate strategy storageAvailable capacityConstraint =
  do
    let
      graph = networkGraph network demandCube processLibrary
    (context'', answer) <-
      foldlM
        (
          \(context, (failure, optimum)) years ->
            do
              let
                timeContext' = (timeContext context) { yearz = years }
                reflow edgeContext =
                  let
                    (flows', cashes', impacts') = costEdge' False strategy years ({- FIXME: Check this. -} zeroFlows timeContext') edgeContext
                  in
                    edgeContext { flows = flows' , cashes = cashes' , impacts = impacts' }
                rebuild PathwayReverseEdge{}  edgeContext = edgeContext
                rebuild (DemandEdge location) edgeContext = edgeContext
                                                            {
                                                              builder    = Nothing
                                                            , capacity   = varyingFlows timeContext' demandCube location
                                                            , reserved   = zeroFlows timeContext'
                                                            , fixed      = []
                                                            , adjustable = Nothing
                                                            , flows      = []
                                                            , cashes     = []
                                                            , impacts    = []
                                                            , reference  = Nothing
                                                            }
                rebuild (ExistingEdge infrastructure) edgeContext =
                                                            let
                                                              existingCube' = existingCube network
                                                            in
                                                              edgeContext
                                                              {
                                                                capacity   = varyingFlows' timeContext' existingCube' infrastructure
                                                              , reserved   = zeroFlows timeContext'
                                                              , adjustable = Nothing
                                                              , flows      = []
                                                              , cashes     = []
                                                              , impacts    = []
                                                              , reference  = Nothing
                                                              }
                rebuild _                     edgeContext = reflow $ edgeContext
                                                            {
                                                              capacity   = case builder edgeContext of
                                                                             Nothing -> capacity edgeContext
                                                                             Just _  -> constantFlows timeContext' [ -- FIXME: Check this.
                                                                                          sum
                                                                                            [
                                                                                              if fYear <: fixed' <= year
                                                                                                then fNameplate <: fixed' * fDutyCycle <: fixed'
                                                                                                else 0
                                                                                            |
                                                                                              fixed' <- construction <$> maybe id (:) (adjustable edgeContext) (fixed edgeContext)
                                                                                            ]
                                                                                        |
                                                                                          year <- years
                                                                                        ]
                                                            , reserved   = zeroFlows timeContext'
                                                            , adjustable = Nothing
                                                            , fixed      = maybe id (:) (adjustable edgeContext) $ fixed edgeContext
                                                            , flows      = []
                                                            , cashes     = []
                                                            , impacts    = []
                                                            , reference  = Nothing
                                                            }
                context' =
                  if years == head yearses
                    then buildContext graph network processLibrary intensityCube priceCube demandCube discountRate strategy
                           $ uncurry (TimeContext years)
                           $ unzip
                           $ ((fPeriod <:) &&& (fDuration <:)) <$> toKnownRecords periodCube
                    else context
                         {
                           timeContext  = timeContext'
                         , edgeContexts = M.mapWithKey rebuild $ edgeContexts context
                         }
              (failure', optimum', context'') <- optimize' storageAvailable graph context'
              return (context'', (failure || failure', optimum <> optimum'))
        )
        (undefined, (False, mempty))
        yearses
    if not storageAvailable
      then return answer
      else do
        when (length yearses > 1)
          $ logCritical "Storage computations only available when there is just a single optimization period."
        logDebug $ show graph
        logDebug $ show answer
        logNotice $ "Cost of subannual variation in flows: " ++
          show (
            sum
              [
                case edge of
                  DemandEdge        {} -> 0
                  ExistingEdge      {} -> totalFlow reserved * sum ((fVariableCost <:) . construction <$> fixed)
                  _                    -> - deltaCost
                                              (strategizing context'')
                                              (discounting context'')
                                              (head yearses)
                                              (levelizeCapacities reserved)
                                              edgeContext
              |
                (edge, edgeContext) <- M.toList $ edgeContexts context''
              , case edgeContext of
                  EdgeContext{..} -> positiveFlow reserved
                  _               -> False
              , let EdgeContext{..} = edgeContext
              ]
          ) ++ " USD."
        sequence_
          [
            logDebug $ show edge
                    ++ "\t" ++ show (storageRequirements reserved)
                    ++ "\t" ++ show (
                                      case edge of
                                        DemandEdge        {} -> 0
                                        ExistingEdge      {} -> - totalFlow reserved * sum ((fVariableCost <:) . construction <$> fixed)
                                        _                    -> deltaCost
                                                                  (strategizing context'')
                                                                  (discounting context'')
                                                                  (head yearses)
                                                                  (levelizeCapacities reserved)
                                                                  edgeContext
                                    )
                    ++ "\t" ++ show (totalFlow reserved)
                    ++ "\t" ++ show reserved
          |
            (edge, edgeContext) <- M.toList $ edgeContexts context''
          , case edgeContext of
              EdgeContext{..} -> positiveFlow reserved
              _               -> False
          , let EdgeContext{..} = edgeContext
          ]
        let
          canStore :: EdgeContext -> Bool
          canStore EdgeContext{..} = any ((/= noStorage) . (fStorage <:) . construction) $ maybe id (:) adjustable fixed
          canStore (EdgeReverseContext edge) = canStore $ edgeContexts context'' M.! edge
          visitVertices :: (Seq Vertex, Set Edge, Set Edge) -> Seq Vertex -> Set Edge -> (Seq Vertex, Set Edge, Set Edge)
          visitVertices (vertexResult, edgeResult, storageResult) pending visited
            | Q.null pending = (vertexResult, edgeResult, storageResult)
            | otherwise      = let
                                 vertex = pending `Q.index` 0
                                 vertexResult' = vertexResult Q.|> vertex
                                 edges =
                                   [
                                     vertexEdge
                                   |
                                     vertexEdge@(_, edge) <- S.toList . M.findWithDefault S.empty vertex $ G.outgoingEdges graph
                                   , case edgeContexts context'' M.! edge of
                                       EdgeContext{..}          -> totalFlow reserved > 0
                                       EdgeReverseContext edge' -> totalFlow (reserved $ edgeContexts context'' M.! edge') < 0
                                   ]
                                 edgeResult' = foldl (flip $ S.insert . snd) edgeResult edges
                                 storageResult' = foldl (flip $ S.insert . snd) storageResult $ filter (canStore . (edgeContexts context'' M.!) . snd) edges
                                 (pending', visited') =
                                   foldl
                                     (
                                       \(pending'', visited'') (vertex', edge) ->
                                         (
                                           if S.singleton edge == S.map snd (M.findWithDefault S.empty vertex' (G.incomingEdges graph)) S.\\ visited''
                                             then pending'' Q.|> vertex'
                                             else pending''
                                         , S.insert edge visited''
                                         )
                                     ) 
                                     (1 `Q.drop` pending, visited)
                                     edges
                               in
                                 visitVertices (vertexResult', edgeResult', storageResult') pending' visited'
          nTimes = length . periods $ timeContext context''
          times = [0..(nTimes-1)]
          lpVertices :: Set     Vertex
          lpEdges    :: Map Int Edge
          lpStorages :: Map Int Edge
          (lpVertices, lpEdges, lpStorages) =
            let
              (vertices, edges, storages) =
                if True
                  then let
                         edges' =
                           S.fromList
                             [
                               edge
                             |
                               edge <- S.toList $ G.allEdges graph
                             , let existing = case edge of
                                                DemandEdge{}   -> True
                                                ExistingEdge{} -> True
                                                _              -> False
                             , let used = case edgeContexts context'' M.! edge of
                                            EdgeContext{..}          -> totalFlow reserved > 0
                                            EdgeReverseContext edge' -> totalFlow (reserved $ edgeContexts context'' M.! edge') < 0
                             , trace'("EDGE\t" ++ show edge ++ "\t" ++ show existing ++ "\t" ++ show used) $ existing || used
                             ]
                         vertices' =
                           [
                             vertex
                           |
                             vertex <- S.toList $ G.allVertices graph
                           , let incoming = [
                                              edge
                                            |
                                              (_, edge) <- S.toList . M.findWithDefault S.empty vertex $ G.incomingEdges graph
                                            , edge `S.member` edges'
                                            ]
                           , let outgoing = [
                                              edge
                                            |
                                              (_, edge) <- S.toList . M.findWithDefault S.empty vertex $ G.outgoingEdges graph
                                            , edge `S.member` edges'
                                            ]
                           , trace'("VERTEX\t" ++ show vertex ++ "\t" ++ show (length incoming) ++ "\t" ++ show (length outgoing)) $ not $ null incoming && null outgoing
                           ]
                         storages' =
                           [
                             edge
                           |
                             edge <- S.toList edges'
                           , let storing = canStore $ edgeContexts context'' M.! edge
                           , trace'("STORAGE\t" ++ show edge ++ "\t" ++ show storing) storing
                           ]
                       in
                         (Q.fromList vertices', edges', S.fromList storages')
                  else visitVertices
                         (Q.empty, S.empty, S.empty)
                         (Q.singleton SuperSource)
                         $ S.fromList
                           [
                             edge
                           |
                             edge <- S.toList $ G.allEdges graph
                           , let existing = case edge of
                                              DemandEdge{}   -> True
                                              ExistingEdge{} -> True
                                              _              -> False
                           , let used = case edgeContexts context'' M.! edge of
                                          EdgeContext{..}          -> totalFlow reserved > 0
                                          EdgeReverseContext edge' -> totalFlow (reserved $ edgeContexts context'' M.! edge') < 0
                           , not $ existing || used
                           ]
            in
              (
                S.fromList             $   toList vertices
              , M.fromList . zip [0..] $ S.toList edges   
              , M.fromList . zip [0..] $ S.toList storages
              )
          offset = nTimes * M.size lpEdges + 1
          disableStorage = False
          lpEdgesInverse    = M.fromList $ swap <$> M.toList lpEdges
          lpStoragesInverse = M.fromList $ swap <$> M.toList lpStorages
          balanceConstraints =
            [
              (
                [
                  1 L.# j
                |   
                  (_, edge) <- incoming
                , let i = lpEdgesInverse M.! edge
                      j = nTimes * i + t + 1
                ]
                ++
                [
                  -1 L.# j
                |   
                  (_, edge) <- outgoing
                , let i = lpEdgesInverse M.! edge
                      j = nTimes * i + t + 1
                ]
                ++
                [
                  if s == 0
                    then  1 L.# j
                    else -1 L.# j
                |
                  not disableStorage
                , (_, edge) <- storages
                , let i = lpStoragesInverse M.! edge
                , s <- [0,1] 
                , let j = 2 * (nTimes * i + t) + s + offset
                ]
              ) L.:==: 0
            |
              vertex <- S.toList lpVertices
            , vertex `notElem` [SuperSource, SuperSink]
            , let incoming = filter (flip M.member lpEdgesInverse    . snd) . S.toList $ G.incomingEdges graph M.! vertex
            , let outgoing = filter (flip M.member lpEdgesInverse    . snd) . S.toList $ G.outgoingEdges graph M.! vertex
            , let storages = filter (flip M.member lpStoragesInverse . snd) incoming
            , t <- times
            ]
          periodicityConstraints =
            [
              [
                if s == 0
                  then  1 L.# j
                  else -1 L.# j
              |
                t <- times
              , s <- [0, 1]
              , let j = 2 * (nTimes * i + t) + s + offset
              ] L.:==: 0
            |
              not disableStorage
            , (i, _) <- M.toList lpStorages
            ]
          flowConstraints =
            concat
              [
                case edge of
                  DemandEdge{}   -> [ [ 1 L.# j] L.:==:  c ]
                  ExistingEdge{} -> [ [ 1 L.# j] L.:<=:  c ]
                  _              -> [
                                      [ 1 L.# j] L.:<=: (c * capacityConstraint)
                                    |
                                      not $ isInfinite capacityConstraint
                                    ]
              |
                (i, edge) <- M.toList lpEdges
              , let VaryingFlows [VaryingFlow cs] = case edgeContexts context'' M.! edge of
                                                      EdgeContext{..}          -> capacity
                                                      EdgeReverseContext edge' -> capacity $ edgeContexts context'' M.! edge'
              , (t, c) <- zip times $ uncurry (*) <$> cs
              , let j = nTimes * i + t + 1
              ]
          availabilityConstraints =
            [
              [
                1 L.# j
              ] L.:==: 0
            |
              not disableStorage
            , ((i, t, s, _, c), _) <- checkStorages
            , isInfinite c
            , let j = 2 * (nTimes * i + t) + s + offset
            ]
          checkEdges =
            [
              (i, t, edge, c, d, f, k, reserved)
            |
              (i, edge) <- M.toList lpEdges
            , let ec@EdgeContext{..} = case edgeContexts context'' M.! edge of
                                         x@EdgeContext{}          -> x
                                         EdgeReverseContext edge' -> edgeContexts context'' M.! edge'
                  f = totalFlow reserved
                  c = costEdge strategy discountRate (head yearses) (zeroFlows $ timeContext context'') ec
                  k = capacity
            , t <- times
            , let VaryingFlows [VaryingFlow rs] = reserved
                  VaryingFlows [VaryingFlow ks] = capacity
                  d = case edge of
                        DemandEdge{}   -> 0
                        ExistingEdge{} -> abs $ c / f
                        _              -> abs $ c / f / (snd (rs !! t) / snd (ks !! t))
            ]
          checkStorages =
            [
              ((i, t, s, edge, p), storageContext)
            |
              (i, edge) <- M.toList lpStorages
            , t <- times
            , s <- [0, 1] :: [Int]
            , let location = case edge of
                               CentralEdge        x _   -> x
                               OnsiteEdge         x _   -> x
                               PathwayForwardEdge x _ _ -> x
                               PathwayReverseEdge x _ _ -> x
                  edgeContext0 = case edgeContexts context'' M.! edge of
                                   x@EdgeContext{}          -> x
                                   EdgeReverseContext edge' -> edgeContexts context'' M.! edge'
                  reserved0 = reserved edgeContext0
                  k = storageRequirements reserved0
                  makeStorage edgeContext@EdgeContext{..} =
                    case (fStorage <:) . construction <$> adjustable of
                      Just (Storage h) -> let
                                            storageContext' = rebuildEdgeContext
                                                                (head yearses)
                                                                (if False then veryConstantFlows (timeContext context'') k else reserved0) -- FIXME
                                                                $ EdgeContext
                                                                  {
                                                                    builder    = Just
                                                                                   $ \_ year' demand ->
                                                                                     uncurry TechnologyContext
                                                                                       <$> technologyReifier
                                                                                             processLibrary
                                                                                             (localize intensityCube location)
                                                                                             (fInfrastructure =: Infrastructure ("STOR-" ++ show i) <+> fLocation =: location)
                                                                                             (head year')
                                                                                             (foldl mostExtreme 0 demand)
                                                                                             0
                                                                                             (Technology h)
                                                                  , capacity   = zeroFlows $ timeContext context''
                                                                  , reserved   = zeroFlows $ timeContext context''
                                                                  , fixed      = []
                                                                  , adjustable = Nothing
                                                                  , flows      = []
                                                                  , cashes     = []
                                                                  , impacts    = []
                                                                  , reference  = Nothing
                                                                  , debit      = 0
                                                                  , pricer     = pricer
                                                                  }
                                            c' =  costEdge strategy discountRate (head yearses) (zeroFlows $ timeContext context'') storageContext'
                                          in
                                            if isInfinite c'
                                              then makeStorage storageContext'
                                              else (storageContext', c')
                      _                -> (edgeContext, inf)
                  (storageContext, c) = makeStorage edgeContext0
                  p = c / if False then k else abs $ totalFlow reserved0
            ]
          problem =
            [
              d
            |
              (_, _, _, _, d, _, _, _) <- checkEdges
            ]
            ++
            [
              if isInfinite c
                then 0
                else c
            |
              not disableStorage
            , ((_, _, _, _, c), _) <- checkStorages
            ]
        case L.simplex (L.Minimize problem) (L.Sparse $ balanceConstraints ++ periodicityConstraints ++ flowConstraints ++ availabilityConstraints) [] of
          L.Optimal (value, solution) -> do
            logDebug    $ "LP Vertices: "                 ++ show lpVertices
            logDebug    $ "LP Edges: "                    ++ show lpEdges
            logDebug    $ "LP Storage: "                  ++ show lpStorages
            logDebug    $ "LP Balance constraints: "      ++ show balanceConstraints
            logDebug    $ "LP Periodicity constraints: "  ++ show periodicityConstraints
            logDebug    $ "LP Flow constraints: "         ++ show flowConstraints
            logDebug    $ "LP Problem: "                  ++ show problem
            logDebug $ "LP MINIMUM = " ++ show value
            sequence_
              [
                logDebug $ "LP EDGE\t"      ++ show (j, i, t)
                               ++ "\t"      ++ show edge
                               ++ "\t SLN " ++ show x
                               ++ "\t FLW " ++ show (uncurry (*) $ rs !! t)
                               ++ "\t CAP " ++ show (uncurry (*) $ ks !! t)
                               ++ "\t CST " ++ show c
                               ++ "\t PRC " ++ show (abs $ c / f)
                               ++ "\t DEL " ++ show d
              |
                (x, (i, t, edge, c, d, f, VaryingFlows [VaryingFlow ks], VaryingFlows [VaryingFlow rs])) <- zip solution checkEdges
              , let j = nTimes * i + t + 1
              ]
            sequence_
              [
                logDebug $ "LP STORAGE\t"     ++ (show (j, i, t, s))
                                  ++ "\t"     ++ show edge
                                  ++ "\tSLN " ++ show x
                                  ++ "\tPRC " ++ show c
              |
                not disableStorage
              , (x, ((i, t, s, edge, c), _)) <- zip (drop (length checkEdges) solution) checkStorages
              , let j = 2 * (nTimes * i + t) + s + offset
              ]
            return
              (
                fst answer
              , M.foldMapWithKey
                  (
                    \i edge ->
                      let
                        (edgeContext, sense) =
                          case edgeContexts context'' M.! edge of
                            ec@EdgeContext{}         -> (ec                               , 1)
                            EdgeReverseContext edge' -> (edgeContexts context'' M.! edge', -1)
                        VaryingFlows [VaryingFlow rs] = reserved edgeContext
                        reserved' =
                          VaryingFlows
                            [
                              VaryingFlow
                                [
                                  (\w -> trace'(  
                                                    "VF\t"       ++ show edge
                                                 ++ "\ti = "     ++ show i
                                                 ++ "\tt = "     ++ show t
                                                 ++ "\tj = "     ++ show j
                                                 ++ "\tx  = "    ++ show x
                                                 ++ "\tsense = " ++ show sense
                                                 ++ "\trs = "    ++ show rs
                                                 ++ "\tw  = "    ++ show w
                                               ) w
                                  )
                                  (df, sense * x / df)
                                |
                                  ((df, _), t) <- zip rs times
                                , let j = nTimes * i + t + 1
                                      x = solution !! (j - 1)
                                ]
                            ]
                        edgeContext' =
                          case edge of
                            DemandEdge{}   -> edgeContext
                            ExistingEdge{} -> edgeContext { reserved = reserved' }
                            _              -> rebuildEdgeContext
                                                (head yearses)
                                                reserved'
                                                edgeContext
                        (flows', cashes', impacts') = (\ww@(w,_,_) -> trace'(show (w, reserved', reserved edgeContext')) ww) $ costEdge' {-# FIXME #-} True strategy (head yearses) (zeroFlows $ timeContext context'') edgeContext'
                      in
                        Optimum (construction <$> maybe id (:) (adjustable edgeContext') (fixed edgeContext')) flows' cashes' impacts'
                  )
                  lpEdges
                <>
                foldMap
                  (
                    \((i, _, _, edge, _), edgeContext) ->
                      let
                        storageFlows =
                          [
                            solution !! (j 0 - 1) - solution !! (j 1 - 1)
                          |
                            t <- times
                          , let j s = 2 * (nTimes * i + t) + s + offset
                          ]
                        storageCumulative = scanl1 (+) storageFlows
                        storageCapacity = maximum storageCumulative - minimum storageCumulative
                        VaryingFlows [VaryingFlow rs] = reserved edgeContext
                        reserved' =
                          (\w -> trace'(
                                            "SF\t"      ++ show edge
                                         ++ "\tstfl = " ++ show storageFlows
                                         ++ "\tstcu = " ++ show storageCumulative
                                         ++ "\tstca = " ++ show storageCapacity
                                         ++ "\tw ="     ++ show w
                                        ) w
                          ) $ VaryingFlows
                            [
                              VaryingFlow
                                [
                                  (df, abs $ x / df) -- FIXME
                                |
                                  ((df, _), x) <- zip rs storageFlows
                                ]
                            ]
                        edgeContext' = (rebuildEdgeContext
                                         (head yearses)
                                         (veryConstantFlows (timeContext context'') storageCapacity)
                                         edgeContext
                                       ) { reserved = reserved' } -- FIXME
                        (flows', cashes', impacts') = (\ww@(w,_,_) -> trace'(show (w, reserved', reserved edgeContext')) ww) $ costEdge' True strategy (head yearses) (zeroFlows $ timeContext context'') edgeContext'
                      in
                        if all ((< 1e-3) . abs) storageFlows
                          then mempty
                          else Optimum (construction <$> maybe id (:) (adjustable edgeContext') (fixed edgeContext')) flows' cashes' impacts'
                  )
                  (filter (\((_, t, s, _, _), _) -> t == 0 && s == 0) checkStorages)
              )
          solution -> do
            logDebug    $ "LP Edges: "                    ++ show lpEdges
            logDebug    $ "LP Storage: "                  ++ show lpStorages
            logDebug    $ "LP Balance constraints: "      ++ show balanceConstraints
            logDebug    $ "LP Periodicity constraints: "  ++ show periodicityConstraints
            logDebug    $ "LP Flow constraints: "         ++ show flowConstraints
            logDebug    $ "LP Problem: "                  ++ show problem
            logCritical $ "Storage optimization failed: " ++ show solution
            return answer



optimize' :: SeraLog m => Bool -> NetworkGraph -> NetworkContext -> m (Bool, Optimum, NetworkContext)
optimize' storageAvailable graph context@NetworkContext{..} =
  do
    let
      TimeContext{..} = timeContext
      NetworkContext _ _ _ _ edgeContexts' =
        reprice 
          $ minimumCostFlow
              (costFunction     storageAvailable yearz)
              (capacityFunction storageAvailable yearz)
              (flowFunction     storageAvailable yearz)
              graph
              context
              SuperSource
              SuperSink
      references =
        M.fromListWith (.+.)
          [
            (location, reserved edgeContext)
          |
            (edge, edgeContext) <- M.toList edgeContexts'
          , let (okay, location) = case edge of
                                     DemandEdge         _             -> (False, undefined)
                                     ExistingEdge       _             -> (False, undefined)
                                     CentralEdge        location' _   -> (False, location')
                                     OnsiteEdge         location' _   -> (False, location')
                                     PathwayForwardEdge location' _ _ -> (True , location')
                                     PathwayReverseEdge {}            -> (False, undefined)
          , okay
          ]
      reflow edgeContext =
        let
          (flows'', cashes'', impacts'') = costEdge' False strategizing yearz ({- FIXME: Check this. -} zeroFlows timeContext) edgeContext
        in
          edgeContext { flows = flows'' , cashes = cashes'' , impacts = impacts'' }
      clear edgeContext@EdgeReverseContext{} = edgeContext
      clear edgeContext                      = reflow $ edgeContext
                                               {
                                                 capacity   = case builder edgeContext of
                                                                Nothing -> capacity edgeContext
                                                                Just _  -> constantFlows timeContext [ -- FIXME: Is the needed?
                                                                             sum
                                                                               [
                                                                                 if fYear <: fixed' <= year
                                                                                   then fNameplate <: fixed' * fDutyCycle <: fixed'
                                                                                   else 0
                                                                               |
                                                                                 fixed' <- construction <$> fixed edgeContext
                                                                               ]
                                                                           |
                                                                             year <- yearz
                                                                           ]
                                               , reserved   = zeroFlows timeContext
                                               , adjustable = Nothing
                                               , flows      = []
                                               , cashes     = []
                                               , impacts    = []
                                               , reference  = Nothing
                                               }
      context'' =
        context
        {
          edgeContexts =
            M.mapWithKey f edgeContexts'
        }
          where
--          f (CentralEdge        location _  ) edgeContext = let
--                                                              edgeContext' = clear edgeContext
--                                                              flow = mostExtreme 1 <$> references M.! location
--                                                            in
--                                                              edgeContext' { reference = Just $ marginalCost years flow edgeContext' }
--          f (OnsiteEdge         location _  ) edgeContext =  let
--                                                              edgeContext' = clear edgeContext
--                                                              flow = mostExtreme 1 <$> references M.! location
--                                                            in
--                                                              edgeContext' { reference = Just $ marginalCost years flow edgeContext' }
            f (PathwayForwardEdge location _ _) edgeContext =  let
                                                                edgeContext' = clear edgeContext
                                                                flow = mostExtreme' 1 $ references M.! location
                                                              in
                                                                edgeContext' { reference = Just $ marginalCost storageAvailable strategizing discounting yearz flow edgeContext' }
            f PathwayReverseEdge{} edgeContext = edgeContext
            f _                    edgeContext = clear edgeContext
      context''' =
        reprice
          $ minimumCostFlow
              (costFunction     storageAvailable yearz)
              (capacityFunction storageAvailable yearz)
              (flowFunction     storageAvailable yearz)
              graph
              context''
              SuperSource
              SuperSink
      (constructions', flows', cashes', impacts') =
        unzip4
          $ concat
          [
            case v of
              EdgeContext{..}      -> [
                                        (
                                          construction <$> maybe id (:) adjustable fixed
                                        , flows
                                        , cashes
                                        , impacts
                                        )
                                      ] 
              EdgeReverseContext _ -> [] 
          |
            (_, v) <- M.toList $ SERA.Infrastructure.Optimization.edgeContexts context'''
          ]
    logInfo ""
    logInfo $ "Optimizing and checking solution for years " ++ show yearz ++ " . . ."
    let
      (existingSupply, totalDemand) =
        ((sum . (totalFlow <$>)) *** (sum . (totalFlow <$>)))
          $ unzip
          [
            case k of
              DemandEdge   _ -> (zeroFlows timeContext        , capacity v)
              ExistingEdge _ -> (capacity v, zeroFlows timeContext        )
              _              -> (zeroFlows timeContext        , zeroFlows timeContext        )
          |
            (k, v) <- M.toList $ SERA.Infrastructure.Optimization.edgeContexts context'''
          ]
      totalCost =
        sum
          $ concat
          [
            case v of
              EdgeContext{..}      -> (fSale <:) <$> flows
              EdgeReverseContext{} -> []
          |
            (_, v) <- M.toList $ SERA.Infrastructure.Optimization.edgeContexts context'''
          ]
    logNotice $ "Total existing production capacity: " ++ show existingSupply            ++ " kg/yr."
    logNotice $ "Total demand: "                       ++ show totalDemand               ++ " kg."
    logNotice $ "Total cost: "                         ++ show totalCost                 ++ " USD."
    logNotice $ "Average cost: "                       ++ show (totalCost / totalDemand) ++ " USD/kg."
    failure <-
      or <$> sequence
        [
          case k of
            DemandEdge   location -> if reserved v #|<# capacity v
                                       then do
                                              logError $ "Unsatisfied demand at \"" ++ show location ++ "\" of " ++ show (capacity v .-. (abs $ reserved v)) ++ " kg/yr."
                                              return True
                                       else do
                                              logDebug $ "Satisfied demand at \"" ++ show location ++ "\" of " ++ show (reserved v) ++ " kg/yr."
                                              return False
            ExistingEdge location -> do
                                       when (reserved v #|<# capacity v)
                                         . logDebug $ "Underused existing production at \"" ++ show location ++ "\" of " ++ show (capacity v .-. (abs $ reserved v)) ++ " kg/yr."
                                       return False
            _                     -> return False
        |
          (k, v) <- M.toList $ SERA.Infrastructure.Optimization.edgeContexts context'''
        , not storageAvailable
        ]
    logInfo " . . . checks complete."
    return
      (
        failure
      , Optimum
          (concat constructions')
          (concat flows')
          (concat cashes')
          (concat impacts')
      , context'''
      )
