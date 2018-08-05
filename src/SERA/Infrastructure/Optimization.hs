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
import Control.Monad.Log (logDebug, logError, logInfo, logNotice)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (evaluate, knownKeys)
import Data.Daft.Vinyl.FieldCube ((!), toKnownRecords, σ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Default.Util (inf)
import Data.Foldable (foldlM)
import Data.Graph.Algorithms (minimumCostFlow)
import Data.Graph.Types (Graph(..), makeGraph)
import Data.List (find, nub, unzip4)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Monoid (Sum(..), (<>))
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
import SERA.Types.Fields (fArea, fCapacity, fCapitalCost, fCost, CostCategory(Salvage), fCostCategory, fDuration, fDutyCycle, fDelivery, fExtended, fFixedCost, fFrom, ImpactCategory(Consumption), fImpactCategory, Infrastructure(..), fInfrastructure, fLifetime, Material, fMaterial, fNameplate, fFuelConsumption, fLength, Location, FLocation, fLocation, fNonFuelConsumption, fSale, fSalvage, Pathway(..), fPathway, Period(..), fPeriod, Productive(..), fProductive, fQuantity, fStage, Technology(..), fTechnology, fTo, fTransmission, fVariableCost, Year, fYear)
import SERA.Types.Records (Cash, Construction, Flow, Impact)

import qualified Data.Map as M
import qualified Data.Set as S


entireYear :: Period
entireYear = Period "Year"


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
            from' = PathwayVertex from pathway stage
            to'   = PathwayVertex to   pathway stage
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
    [
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
                             (flows', cashes', impacts') = costEdge' strategizing yearz (zeroFlows timeContext) edgeContext
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
                                            (flows', cashes', impacts') = costEdge' strategizing yearz ({- FIXME: Checkt this. -} zeroFlows timeContext) edgeContext
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
                                                             existing = existingCube ! (fInfrastructure =: infrastructure)
                                                           in
                                                             EdgeContext
                                                             {
                                                               builder    = Nothing
                                                             , capacity   = constantFlows timeContext -- FIXME: Move to a function.
                                                                              [
                                                                                if fYear <: existing <= year
                                                                                  then fCapacity <: existing
                                                                                  else 0
                                                                              |
                                                                                year <- yearz
                                                                              ]
                                                             , reserved   = zeroFlows timeContext
                                                             , fixed      = [
                                                                              let
                                                                                construction =     fInfrastructure =: infrastructure
                                                                                               <+> fLocation       =: fLocation <: existing
                                                                                               <+> fTechnology     =: fTechnology <: existing
                                                                                               <+> fProductive     =: Central
                                                                                               <+> fYear           =: fYear <: existing
                                                                                               <+> fLifetime       =: 1000
                                                                                               <+> fNameplate      =: fCapacity <: existing
                                                                                               <+> fDutyCycle      =: 1
                                                                                               <+> fLength         =: 0
                                                                                               <+> fCapitalCost    =: 0
                                                                                               <+> fFixedCost      =: 0
                                                                                               <+> fVariableCost   =: fCost <: existing
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
    . costEdge' strategy year delta -- FIXME: Handle salvage.


costEdge' :: Strategy -> [Year] -> VaryingFlows -> EdgeContext -> ([Flow], [Cash], [Impact])
costEdge' strategy year delta EdgeContext{..} =
  let
    technologyContexts = maybe id (:) adjustable fixed
    (flows', cashes', impacts') = unzip3 . zipWith (\year' flow' -> costTechnologies pricer year' flow' technologyContexts) year . unvaryingFlows $ reserved .+. delta
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
costEdge' _ _ _ _ = error "costEdge': edge is reversed."


costTechnologies :: Pricer -> Year -> VaryingFlow -> [TechnologyContext] -> ([Flow], [Cash], [Impact])
costTechnologies pricer year flow technologyContexts =
  let
    (flows', cashes', impacts', _) = -- FIXME: check residue
      foldr (\t (flows'', cashes'', impacts'', residue'') -> let
                                                               (flows''', cashes''', impacts''', residue''') = costTechnology pricer year residue'' t
                                                             in
                                                               (flows''' ++ flows'', cashes''' ++ cashes'', impacts''' ++ impacts'', residue''')
            )
            ([], [], [], flow)
            technologyContexts
  in
    (flows', cashes', impacts')


costTechnology :: Pricer -> Year -> VaryingFlow -> TechnologyContext -> ([Flow], [Cash], [Impact], VaryingFlow) -- TODO: Move into operations, which will return flow and residue.
costTechnology pricer year flow TechnologyContext{..} =                                            -- FIXME: Guard against overflow.
  let
    capacity' = fNameplate <: construction * fDutyCycle <: construction
    flow' =
      if peakFlow flow <= capacity'
        then flow
        else signum flow #* capacity'
    residue = flow - flow'
    (flows', cashes', impacts', _) = operation pricer year $ sumFlow flow'
  in
    if year < fYear <: construction
      then ([], [], [], flow)
      else ([flows'], cashes', impacts', residue)


marginalCost :: Strategy -> Double -> [Year] -> VaryingFlows -> EdgeContext -> Double
marginalCost strategy discount year delta edgeContext =
  fromMaybe
    (
      let
        oldCost =                 costEdge strategy discount year (zeroFlows' delta)                                    edgeContext
        newCost = fromMaybe inf $ costEdge strategy discount year (zeroFlows' delta) <$> adjustEdge strategy year delta edgeContext
      in
        if True
          then (newCost - oldCost) / sumAbs delta
          else newCost / sumAbs (reserved edgeContext .+. delta)
    )
    (reference edgeContext)


adjustEdge :: Strategy -> [Year] -> VaryingFlows -> EdgeContext -> Maybe EdgeContext
adjustEdge strategy years delta edgeContext@EdgeContext{..} =
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
      if reserved' #&<=# capacity
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
      (flows', cashes', impacts') = costEdge' strategy years (zeroFlows' delta) edgeContext'
    return
      $ edgeContext'
        {
          flows = flows'
        , cashes = cashes'
        , impacts = impacts'
        }
adjustEdge _ _ _ _ = error "adjustCapacity: edge is reversed."


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
  Capacity x `compare` Capacity y = minimumOfVaryingFlows y `compare` minimumOfVaryingFlows x

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


costFunction :: [Year] -> Capacity -> NetworkContext -> Edge -> Maybe (Sum Double, NetworkContext)
costFunction year (Capacity flow) context edge =
  do
    let
      edgeContext = edgeContexts context M.! edge
      (capacity', builder') =
        case edgeContext of
          EdgeContext{..}          -> (capacity .-. (absFlows reserved), builder)
          EdgeReverseContext edge' -> (\z -> capacity z .-. (absFlows $ reserved z)) &&& builder $ edgeContexts context M.! edge'
      canBuild =  canBuild' year flow builder'
    guard
      $ (zeroFlows' capacity') #<# capacity' || canBuild
    case edge of
      DemandEdge _                              -> return (Sum $ debit edgeContext, context)
      ExistingEdge _                            -> return (Sum . sum $ (fVariableCost <:) . construction <$> fixed edgeContext, context)
      PathwayReverseEdge location pathway stage -> costFunction year (Capacity (negate flow)) context $ PathwayForwardEdge location pathway stage
      _                                         -> return (Sum $ marginalCost (strategizing context) (discounting context) year flow edgeContext, context)
costFunction _ _ _ _ = Nothing -- error "costFunction: no flow."


capacityFunction :: [Year] -> NetworkContext -> Edge -> Maybe (Capacity, NetworkContext)
capacityFunction year context edge =
  case edgeContexts context M.! edge of
    EdgeContext{..}          -> do
                                  let
                                    canBuild =  canBuild' year (veryConstantFlows (timeContext context) 1) builder
                                  guard
                                    $ reserved #<# capacity || canBuild
                                  return
                                    (
                                      if canBuild
                                        then Capacity $ veryConstantFlows (timeContext context) inf
                                        else Capacity $ capacity .-. abs reserved
                                    , context
                                    )
    EdgeReverseContext edge' -> capacityFunction year context edge'


canBuild' :: [Year] -> VaryingFlows -> Maybe TechnologyBuilder -> Bool
canBuild' _    flow Nothing        = maximumAbs flow == 0
canBuild' year flow (Just builder) = isJust $ builder 0 year $ peakAnnualFlows flow


flowFunction :: [Year] -> Capacity -> NetworkContext -> Edge -> NetworkContext
flowFunction year (Capacity flow) context edge =
  fromMaybe (trace ("FAILED TO SET FLOW\t" ++ show edge ++ "\t" ++ show (construction <$> adjustable (edgeContexts context M.! edge)) ++ "\t" ++ show (construction <$> fixed (edgeContexts context M.! edge)) ++ "\t" ++ show year ++ "\t" ++ show flow) context)
    $ do
        let
          edgeContext = edgeContexts context M.! edge
          update edgeContext' = context { edgeContexts = M.insert edge (edgeContext' { reference = Nothing } ) $ edgeContexts context }
          (capacity', builder') =
            case edgeContext of
              EdgeContext{..}          -> (capacity .-. abs reserved, builder)
              EdgeReverseContext edge' -> (\z -> capacity z .-. abs (reserved z)) &&& builder $ edgeContexts context M.! edge'
          canBuild =  canBuild' year flow builder'
        guard
          $ (zeroFlows $ timeContext context) #<# capacity' || canBuild
        case edge of
          DemandEdge _                              -> return . update $ edgeContext { reserved = reserved edgeContext .+. flow}
          ExistingEdge _                            -> return . update $ let
                                                                  (flows', cashes', impacts') = costEdge' (strategizing context) year (zeroFlows $ timeContext context) $ edgeContext { reserved = reserved edgeContext .+. flow }
                                                                in
                                                                  edgeContext
                                                                  {
                                                                    reserved = reserved edgeContext .+. flow
                                                                  , flows    = flows'
                                                                  , cashes   = cashes'
                                                                  , impacts  = impacts'
                                                                  }
          PathwayReverseEdge location pathway stage -> return $ flowFunction year (Capacity (negate flow)) context $ PathwayForwardEdge location pathway stage
          _                                         -> update <$> adjustEdge (strategizing context) year flow edgeContext
flowFunction _ _ _ _ = error "flowFunction: no flow."


optimize :: SeraLog m => [[Year]] -> PeriodCube -> Network -> DemandCube -> IntensityCube '[FLocation] -> ProcessLibrary -> PriceCube '[FLocation] -> Double -> Double -> Strategy -> m (Bool, Optimum)
optimize yearses periodCube network demandCube intensityCube processLibrary priceCube discountRate _escalationRate strategy =
  do
    let
      graph = networkGraph network demandCube processLibrary
    snd
      <$> foldlM
        (
          \(context, (failure, optimum)) years ->
            do
              let
                reflow edgeContext =
                  let
                    (flows', cashes', impacts') = costEdge' strategy years ({- FIXME: Check this. -} zeroFlows $ timeContext context) edgeContext
                  in
                    edgeContext { flows = flows' , cashes = cashes' , impacts = impacts' }
                rebuild PathwayReverseEdge{}  edgeContext = edgeContext
                rebuild (DemandEdge location) edgeContext = edgeContext
                                                            {
                                                              builder    = Nothing
                                                            , capacity   = constantFlows (timeContext context) [
                                                                             maybe 0 (\rec -> fFuelConsumption <: rec + fNonFuelConsumption <: rec)
                                                                               $ demandCube `evaluate` (fLocation =: location <+> fYear =: year <+> fPeriod =: entireYear)
                                                                           |
                                                                             year <- years
                                                                           ] -- FIXME: Is this needed?
                                                            , reserved   = zeroFlows $ timeContext context
                                                            , fixed      = []
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
                                                                             Just _  -> constantFlows (timeContext context) [
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
                                                            , reserved   = zeroFlows $ timeContext context
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
                           timeContext  = (timeContext context) { yearz = years }
                         , edgeContexts = M.mapWithKey rebuild $ edgeContexts context
                         }
              (failure', optimum', context'') <- optimize' graph context'
              return (context'', (failure || failure', optimum <> optimum'))
        )
        (undefined, (False, mempty))
        yearses



optimize' :: SeraLog m => NetworkGraph -> NetworkContext -> m (Bool, Optimum, NetworkContext)
optimize' graph context@NetworkContext{..} =
  do
    let
      TimeContext{..} = timeContext
      NetworkContext _ _ _ _ edgeContexts' =
        reprice 
          $ minimumCostFlow
              (costFunction     yearz)
              (capacityFunction yearz)
              (flowFunction     yearz)
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
          (flows'', cashes'', impacts'') = costEdge' strategizing yearz ({- FIXME: Check this. -} zeroFlows timeContext) edgeContext
        in
          edgeContext { flows = flows'' , cashes = cashes'' , impacts = impacts'' }
      clear edgeContext@EdgeReverseContext{} = edgeContext
      clear edgeContext                      = reflow $ edgeContext
                                               {
                                                 capacity   = case builder edgeContext of
                                                                Nothing -> capacity edgeContext
                                                                Just _  -> constantFlows timeContext [
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
                                                                edgeContext' { reference = Just $ marginalCost strategizing discounting yearz flow edgeContext' }
            f PathwayReverseEdge{} edgeContext = edgeContext
            f _                    edgeContext = clear edgeContext
      context''' =
        reprice
          $ minimumCostFlow
              (costFunction     yearz)
              (capacityFunction yearz)
              (flowFunction     yearz)
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
