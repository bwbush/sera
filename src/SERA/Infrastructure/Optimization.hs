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
import Data.Daft.DataCube (evaluable, evaluate, knownKeys)
import Data.Daft.Vinyl.FieldCube ((!), toKnownRecords, σ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Default.Util (inf)
import Data.Foldable (foldlM)
import Data.Graph.Algorithms (minimumCostFlow)
import Data.Graph.Types (Graph(..), makeGraph)
import Data.List (find, nub, unzip4)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Monoid (Sum(..), (<>))
import Data.Tuple.Util (snd3)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import SERA (SeraLog)
import SERA.Material (Pricer, localize)
import SERA.Network (Network(..))
import SERA.Process.Reification (TechnologyOperation, operationReifier, technologyReifier)
import SERA.Process (ProcessLibrary(..), isProduction)
import SERA.Types.Cubes (DemandCube, IntensityCube, PriceCube)
import SERA.Types.Fields (fCapacity, fCapitalCost, fCost, CostCategory(Salvage), fCostCategory, fDutyCycle, fExtended, fFixedCost, fFrom, Infrastructure(..), fInfrastructure, fLifetime, fNameplate, fFuelConsumption, fLength, Location, FLocation, fLocation, fMaterial, fNonFuelConsumption, fPrice, fSale, fSalvage, Pathway(..), fPathway, Productive(..), fProductive, fStage, Technology(..), fTechnology, fTo, fTransmission, fVariableCost, Year, fYear)
import SERA.Types.Records (Cash, Construction, Flow, Impact)

import qualified Data.Map as M
import qualified Data.Set as S


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
      case compare stage extendedStage of
        LT -> [
                (
                  if stage == firstStage
                    then ProductionVertex location
                    else PathwayVertex location pathway stage
                , PathwayVertex location pathway $ stage + 1
                , PathwayForwardEdge location pathway stage
                )
              |
                node <- toKnownRecords nodeCube
              , let location = fLocation <: node
              ]
        EQ -> concat
              [
                [
                  (from', to'  , PathwayForwardEdge location pathway stage) 
                , (to'  , from', PathwayReverseEdge location pathway stage) 
                ]
              |
                link <- toKnownRecords linkCube
              , let location = fLocation <: link
                    from = fFrom <: link
                    to   = fTo   <: link
                    from' = PathwayVertex from pathway stage
                    to'   = PathwayVertex to   pathway stage
              ]
        GT -> [
                (
                  PathwayVertex location pathway $ stage - 1
                , if stage == lastStage
                    then ConsumptionVertex location  -- FIXME: Only do this when there actually is consumption.
                    else PathwayVertex location pathway stage
                , PathwayForwardEdge location pathway stage
                )
              |
                node <- toKnownRecords nodeCube
              , let location = fLocation <: node
              ]
    |
      pathway <- S.toList $ S.map (fPathway <:) $ knownKeys pathwayCube
    , let stages = toKnownRecords $ σ (\key _ -> pathway == fPathway <: key) pathwayCube
          extendedStage' = find (\rec -> fExtended <: rec && fTransmission <: rec) stages
          firstStage = fStage <: minimum stages
          lastStage = fStage <: maximum stages
    , isJust extendedStage'
    , let extendedStage = fStage <: fromJust extendedStage'
    , stage <- (fStage <:) <$> stages
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
    , capacity   :: [Double]
    , reserved   :: [Double]
    , fixed      :: [TechnologyContext]
    , adjustable :: Maybe TechnologyContext
    , flows      :: [Flow]
    , cashes     :: [Cash]
    , impacts    :: [Impact]
    , reference  :: Maybe Double
    }
  | EdgeReverseContext Edge


data NetworkContext =
  NetworkContext
  {
    strategizing :: Strategy
  , discounting  :: Double
  , edgeContexts :: Map Edge EdgeContext
  }


buildContext :: NetworkGraph -> Network -> ProcessLibrary -> IntensityCube '[FLocation] -> PriceCube '[FLocation] -> DemandCube -> Double -> Strategy -> [Year] -> NetworkContext
buildContext Graph{..} Network{..} processLibrary@ProcessLibrary{..} intensityCube priceCube demandCube discounting strategizing years =
  let
    edgeContexts =
      M.fromList
        [
          (
            edge
          , (
              \edgeContext ->
                case edge of
                  DemandEdge{}         -> edgeContext
                  PathwayReverseEdge{} -> edgeContext
                  _                    -> let
                                            (flows', cashes', impacts') = costEdge' LiteralInWindow years (reserved edgeContext) edgeContext
                                          in
                                            edgeContext { flows = flows' , cashes = cashes' , impacts = impacts' }
            ) $
            case edge of
              DemandEdge location                       -> EdgeContext
                                                           {
                                                             builder    = Nothing
                                                           , capacity   = [
                                                                            maybe 0 (\rec -> fFuelConsumption <: rec + fNonFuelConsumption <: rec)
                                                                              $ demandCube `evaluate` (fLocation =: location <+> fYear =: year)
                                                                          |
                                                                            year <- years
                                                                          ]
                                                           , reserved   = const 0 <$> years
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
                                                           , reference  = Nothing
                                                           }
              ExistingEdge infrastructure               -> let
                                                             existing = existingCube ! (fInfrastructure =: infrastructure)
                                                           in
                                                             EdgeContext
                                                             {
                                                               builder    = Nothing
                                                             , capacity   = [
                                                                              if fYear <: existing <= year
                                                                                then fCapacity <: existing
                                                                                else 0
                                                                            |
                                                                              year <- years
                                                                            ]
                                                             , reserved   = const 0 <$> years
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
                                                                                                   (makePricer priceCube $ fLocation <: existing)
                                                                                                   construction
                                                                                }
                                                                            ]
                                                             , adjustable = Nothing
                                                             , flows      = []
                                                             , cashes     = []
                                                             , impacts    = []
                                                             , reference  = Nothing
                                                             }
              CentralEdge location technology           -> EdgeContext
                                                           {
                                                             builder    = Just
                                                                            $ \i year' demand ->
                                                                              fmap (uncurry TechnologyContext)
                                                                              $ technologyReifier
                                                                                  processLibrary
                                                                                  (localize intensityCube location)
                                                                                  (makePricer priceCube location)
                                                                                  (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show (head year') ++ "-" ++ show i) <+> fLocation =: location)
                                                                                  (minimum $ last year' : catMaybes (zipWith (\year'' demand'' -> guard (demand'' /= 0) >> return year'') year' demand))
                                                                                  (foldl mostExtreme 0 demand)
                                                                                  0
                                                                                  technology
                                                           , capacity   = const 0 <$> years
                                                           , reserved   = const 0 <$> years
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
                                                           , reference  = Nothing
                                                           }
              OnsiteEdge location technology            -> EdgeContext
                                                           {
                                                             builder    = Just
                                                                            $ \i year' demand ->
                                                                              fmap (uncurry TechnologyContext)
                                                                              $ technologyReifier
                                                                                  processLibrary
                                                                                  (localize intensityCube location)
                                                                                  (makePricer priceCube location)
                                                                                  (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show (head year') ++ "-" ++ show i) <+> fLocation =: location)
                                                                                  (minimum $ last year' : catMaybes (zipWith (\year'' demand'' -> guard (demand'' /= 0) >> return year'') year' demand))
                                                                                  (foldl mostExtreme 0 demand)
                                                                                  0
                                                                                  technology
                                                           , capacity   = const 0 <$> years
                                                           , reserved   = const 0 <$> years
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
                                                           , reference  = Nothing
                                                           }
              PathwayForwardEdge location pathway stage -> let
                                                             technology = fTechnology <: (pathwayCube ! (fPathway =: pathway <+> fStage =: stage))
                                                             (from, distance) =
                                                               if linkCube `evaluable` (fLocation =: location)
                                                                 then let
                                                                        link = linkCube ! (fLocation =: location)
                                                                      in
                                                                        (fFrom <: link, fLength <: link)
                                                                 else (location, 0)
                                                           in
                                                             EdgeContext
                                                             {
                                                               builder    = Just
                                                                              $ \i year' demand ->
                                                                                fmap (uncurry TechnologyContext)
                                                                                $ technologyReifier
                                                                                    processLibrary
                                                                                    (localize intensityCube location)
                                                                                    (makePricer priceCube from)
                                                                                    (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show (head year') ++ "-" ++ show i) <+> fLocation =: location)
                                                                                    (minimum $ last year' : catMaybes (zipWith (\year'' demand'' -> guard (demand'' /= 0) >> return year'') year' demand))
                                                                                    (foldl mostExtreme 0 demand)
                                                                                    distance
                                                                                    technology
                                                             , capacity   = const 0 <$> years
                                                             , reserved   = const 0 <$> years
                                                             , fixed      = []
                                                             , adjustable = Nothing
                                                             , flows      = []
                                                             , cashes     = []
                                                             , impacts    = []
                                                             , reference  = Nothing
                                                             }
              PathwayReverseEdge location pathway stage -> EdgeReverseContext $ PathwayForwardEdge location pathway stage
          )
        |
          (identifier, edge) <- zip [(1::Int)..] $ S.toList allEdges
        ]
  in
    NetworkContext{..}


(.+.) :: Num a => [a] -> [a] -> [a]
(.+.) = zipWith (+)


(.-.) :: Num a => [a] -> [a] -> [a]
(.-.) = zipWith (-)


(#|<#) :: (Num a, Ord a) => [a] -> [a] -> Bool
(#|<#) = (or .) . zipWith ((<) . abs)


(#&<=#) :: (Num a, Ord a) => [a] -> [a] -> Bool
(#&<=#) = (and .) . zipWith ((<=) . abs)


(#<#) :: (Num a, Ord a) => [a] -> [a] -> Bool
(#<#) x y =
  let
    x' = abs <$> x
  in
    x' /= y && and (zipWith (<=) x' y)


costEdge :: Strategy -> Double -> [Year] -> [Double] -> EdgeContext -> Double
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


costEdge' :: Strategy -> [Year] -> [Double] -> EdgeContext -> ([Flow], [Cash], [Impact])
costEdge' strategy year delta EdgeContext{..} =
  let
    technologyContexts = (maybe id (:) adjustable) fixed
    (flows', cashes', impacts') = unzip3 . zipWith (\year' flow' -> costTechnologies year' flow' technologyContexts) year $ reserved .+. delta
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


costTechnologies :: Year -> Double -> [TechnologyContext] -> ([Flow], [Cash], [Impact])
costTechnologies year flow technologyContexts =
  let
    (flows', cashes', impacts', _) = -- FIXME: check residue
      foldr (\t (flows'', cashes'', impacts'', residue'') -> let
                                                               (flows''', cashes''', impacts''', residue''') = costTechnology year residue'' t
                                                             in
                                                               (flows''' ++ flows'', cashes''' ++ cashes'', impacts''' ++ impacts'', residue''')
            )
            ([], [], [], flow)
            technologyContexts
  in
    (flows', cashes', impacts')


costTechnology :: Year -> Double -> TechnologyContext -> ([Flow], [Cash], [Impact], Double) -- TODO: Move into operations, which will return flow and residue.
costTechnology year flow TechnologyContext{..} =                                            -- FIXME: Guard against overflow.
  let
    capacity' = fNameplate <: construction * fDutyCycle <: construction
    flow' =
      if abs flow <= capacity'
        then flow
        else signum flow * capacity'
    residue = flow - flow'
    (flows', cashes', impacts', _) = operation year flow'
  in
    if year < fYear <: construction
      then ([], [], [], flow)
      else ([flows'], cashes', impacts', residue)


sumAbs :: (Num a, Functor t, Foldable t) => t a -> a
sumAbs = sum . fmap abs


maximumAbs :: (Num a, Ord a, Functor t, Foldable t) => t a -> a
maximumAbs = maximum . fmap abs


marginalCost :: Strategy -> Double -> [Year] -> [Double] -> EdgeContext -> Double
marginalCost strategy discount year delta edgeContext =
  case reference edgeContext of
    Just x  -> x
    Nothing -> let
                 oldCost =                 costEdge strategy discount year (const 0 <$> year)                           edgeContext
                 newCost = fromMaybe inf $ costEdge strategy discount year (const 0 <$> year) <$> adjustEdge year delta edgeContext
               in
                 if True
                   then (newCost - oldCost) / sumAbs delta
                   else newCost / sumAbs (reserved edgeContext .+. delta)


adjustEdge :: [Year] -> [Double] -> EdgeContext -> Maybe EdgeContext
adjustEdge years delta edgeContext@EdgeContext{..} =
  do
    let
      reserved' = reserved .+. delta
      without = [
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
               adjustable' <-
                 builder'
                   (length fixed + 1)
                   years
                   [
                     signum y * maximum [0, abs y - x]
                   |
                     (x, y) <- zip without reserved'
                   ]
               return
                 $ edgeContext
                   {
                     capacity = [
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
      (flows', cashes', impacts') = costEdge' LiteralInWindow years (const 0 <$> years) edgeContext'
    return
      $ edgeContext'
        {
          flows = flows'
        , cashes = cashes'
        , impacts = impacts'
        }
adjustEdge _ _ _ = error "adjustCapacity: edge is reversed."


makePricer :: PriceCube '[FLocation] -> Location -> Pricer
makePricer priceCube location material year =
  maybe 0 (fPrice <:)
    $ priceCube `evaluate` (fMaterial =: material <+> fYear =: year <+> fLocation =: location) -- FIXME: extrapolate


mostExtreme :: (Num a, Ord a) => a -> a -> a
mostExtreme x y =
  if abs x >= abs y
    then x
    else y


data Capacity a = Capacity [a] | Unlimited
  deriving (Read, Show)

instance (RealFloat a, Num a, Ord a) => Eq (Capacity a) where
  Unlimited  == Unlimited  = True
  Unlimited  == Capacity y = all isInfinite y
  Capacity x == Unlimited  = all isInfinite x
  Capacity x == Capacity y = minimum y == minimum x

instance (RealFloat a, Num a, Ord a) => Ord (Capacity a) where
  Unlimited  `compare` Unlimited  = EQ
  Unlimited  `compare` Capacity y = if all isInfinite y then EQ else LT
  Capacity x `compare` Unlimited  = if all isInfinite x then EQ else GT
  Capacity x `compare` Capacity y = minimum y `compare` minimum x

instance Functor Capacity where
  fmap _ Unlimited    = Unlimited
  fmap f (Capacity x) = Capacity $ fmap f x

instance (Num a, Ord a) => Monoid (Capacity a) where
  mempty = Unlimited
  Unlimited  `mappend` Unlimited    = Unlimited
  Unlimited  `mappend` (Capacity y) = Capacity y
  Capacity x `mappend` Unlimited    = Capacity x
  Capacity x `mappend` Capacity y   = Capacity $ minimum [x, y]


costFunction :: [Year] -> Capacity Double -> NetworkContext -> Edge -> Maybe (Sum Double, NetworkContext)
costFunction year (Capacity flow) context edge =
  do
    let
      edgeContext = edgeContexts context M.! edge
      (capacity', builder') =
        case edgeContext of
          EdgeContext{..}          -> (capacity .-. (abs <$> reserved), builder)
          EdgeReverseContext edge' -> (\z -> capacity z .-. (abs <$> reserved z)) &&& builder $ edgeContexts context M.! edge'
      canBuild =  canBuild' year flow builder'
    guard
      $ (const 0 <$> year) #<# capacity' || canBuild
    case edge of
      DemandEdge _                              -> return (Sum 0, context)
      ExistingEdge _                            -> return (Sum . sum $ (fVariableCost <:) . construction <$> fixed edgeContext, context)
      PathwayReverseEdge location pathway stage -> costFunction year (Capacity (negate <$> flow)) context $ PathwayForwardEdge location pathway stage
      _                                         -> return (Sum $ marginalCost (strategizing context) (discounting context) year flow edgeContext, context)
costFunction _ _ _ _ = Nothing -- error "costFunction: no flow."


capacityFunction :: [Year] -> NetworkContext -> Edge -> Maybe (Capacity Double, NetworkContext)
capacityFunction year context edge =
  case edgeContexts context M.! edge of
    EdgeContext{..}          -> do
                                  let
                                    canBuild =  canBuild' year (const 1 <$> year) builder
                                  guard
                                    $ reserved #<# capacity || canBuild
                                  return
                                    (
                                      if canBuild
                                        then Capacity $ const inf <$> reserved
                                        else Capacity $ capacity .-. (abs <$> reserved)
                                    , context
                                    )
    EdgeReverseContext edge' -> capacityFunction year context edge'


canBuild' :: [Year] -> [Double] -> Maybe TechnologyBuilder -> Bool
canBuild' _    flow Nothing        = maximumAbs flow == 0
canBuild' year flow (Just builder) = isJust $ builder 0 year flow


flowFunction :: [Year] -> Capacity Double -> NetworkContext -> Edge -> NetworkContext
flowFunction year (Capacity flow) context edge =
  fromMaybe (trace ("FAILED TO SET FLOW\t" ++ show edge ++ "\t" ++ show (construction <$> adjustable (edgeContexts context M.! edge)) ++ "\t" ++ show (construction <$> fixed (edgeContexts context M.! edge)) ++ "\t" ++ show year ++ "\t" ++ show flow) context)
    $ do
        let
          edgeContext = edgeContexts context M.! edge
          update edgeContext' = context { edgeContexts = M.insert edge (edgeContext' { reference = Nothing } ) $ edgeContexts context }
          (capacity', builder') =
            case edgeContext of
              EdgeContext{..}          -> (capacity .-. (abs <$> reserved), builder)
              EdgeReverseContext edge' -> (\z -> capacity z .-. (abs <$> reserved z)) &&& builder $ edgeContexts context M.! edge'
          canBuild =  canBuild' year flow builder'
        guard
          $ (const 0 <$> year) #<# capacity' || canBuild
        case edge of
          DemandEdge _                              -> return . update $ edgeContext { reserved = reserved edgeContext .+. flow}
          ExistingEdge _                            -> return . update $ let
                                                                  (flows', cashes', impacts') = costEdge' LiteralInWindow year (const 0 <$> year) $ edgeContext { reserved = reserved edgeContext .+. flow }
                                                                in
                                                                  edgeContext
                                                                  {
                                                                    reserved = reserved edgeContext .+. flow
                                                                  , flows    = flows'
                                                                  , cashes   = cashes'
                                                                  , impacts  = impacts'
                                                                  }
          PathwayReverseEdge location pathway stage -> return $ flowFunction year (Capacity (negate <$> flow)) context $ PathwayForwardEdge location pathway stage
          _                                         -> update <$> adjustEdge year flow edgeContext
flowFunction _ _ _ _ = error "flowFunction: no flow."


optimize :: SeraLog m => [[Year]] -> Network -> DemandCube -> IntensityCube '[FLocation] -> ProcessLibrary -> PriceCube '[FLocation] -> Double -> Double -> Strategy -> m (Bool, Optimum)
optimize yearses network demandCube intensityCube processLibrary priceCube discountRate escalationRate strategy =
  do
    let
      graph = networkGraph network demandCube processLibrary
    fmap snd
      $ foldlM
        (
          \(context, (failure, optimum)) years ->
            do
              let
                reflow edgeContext =
                  let
                    (flows', cashes', impacts') = costEdge' LiteralInWindow years (reserved edgeContext) edgeContext
                  in
                    edgeContext { flows = flows' , cashes = cashes' , impacts = impacts' }
                rebuild PathwayReverseEdge{}  edgeContext = edgeContext
                rebuild (DemandEdge location) _           = EdgeContext
                                                            {
                                                              builder    = Nothing
                                                            , capacity   = [
                                                                             maybe 0 (\rec -> fFuelConsumption <: rec + fNonFuelConsumption <: rec)
                                                                               $ demandCube `evaluate` (fLocation =: location <+> fYear =: year)
                                                                           |
                                                                             year <- years
                                                                           ]
                                                            , reserved   = const 0 <$> years
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
                                                                             Just _  -> [
                                                                                          sum
                                                                                            [
                                                                                              if fYear <: fixed' <= year
                                                                                                then fNameplate <: fixed' * fDutyCycle <: fixed'
                                                                                                else 0
                                                                                            |
                                                                                              fixed' <- construction <$> (maybe id (:) (adjustable edgeContext)) (fixed edgeContext)
                                                                                            ]
                                                                                        |
                                                                                          year <- years
                                                                                        ]
                                                            , reserved   = const 0 <$> years
                                                            , adjustable = Nothing
                                                            , fixed      = maybe id (:) (adjustable edgeContext) $ fixed edgeContext
                                                            , flows      = []
                                                            , cashes     = []
                                                            , impacts    = []
                                                            , reference  = Nothing
                                                            }
                context' =
                  if years == head yearses
                    then buildContext graph network processLibrary intensityCube priceCube demandCube discountRate strategy years
                    else NetworkContext strategy discountRate $ M.mapWithKey rebuild $ edgeContexts context
              (failure', optimum', context'') <- optimize' years graph context'
              return (context'', (failure || failure', optimum <> optimum'))
        )
        (undefined, (False, mempty))
        $ yearses



optimize' :: SeraLog m => [Year] -> NetworkGraph -> NetworkContext -> m (Bool, Optimum, NetworkContext)
optimize' years graph context =
  do
    let
      NetworkContext strategy discountRate edgeContexts' =
        minimumCostFlow
          (costFunction years)
          (capacityFunction years)
          (flowFunction years)
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
                                     PathwayReverseEdge _         _ _ -> (False, undefined)
          , okay
          ]
      reflow edgeContext =
        let
          (flows'', cashes'', impacts'') = costEdge' LiteralInWindow years (reserved edgeContext) edgeContext
        in
          edgeContext { flows = flows'' , cashes = cashes'' , impacts = impacts'' }
      clear edgeContext@EdgeReverseContext{} = edgeContext
      clear edgeContext                      = reflow $ edgeContext
                                               {
                                                 capacity   = case builder edgeContext of
                                                                Nothing -> capacity edgeContext
                                                                Just _  -> [
                                                                             sum
                                                                               [
                                                                                 if fYear <: fixed' <= year
                                                                                   then fNameplate <: fixed' * fDutyCycle <: fixed'
                                                                                   else 0
                                                                               |
                                                                                 fixed' <- construction <$> fixed edgeContext
                                                                               ]
                                                                           |
                                                                             year <- years
                                                                           ]
                                               , reserved   = const 0 <$> years
                                               , adjustable = Nothing
                                               , flows      = []
                                               , cashes     = []
                                               , impacts    = []
                                               , reference  = Nothing
                                               }
      context'' =
        NetworkContext strategy discountRate $ M.mapWithKey f edgeContexts'
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
                                                                flow = mostExtreme 1 <$> references M.! location
                                                              in
                                                                edgeContext' { reference = Just $ marginalCost strategy discountRate years flow edgeContext' }
            f (PathwayReverseEdge _        _ _) edgeContext = edgeContext
            f _                                 edgeContext = clear edgeContext
      context'''@NetworkContext{..} =
        minimumCostFlow
          (costFunction years)
          (capacityFunction years)
          (flowFunction years)
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
                                          construction <$> (maybe id (:) adjustable) fixed
                                        , flows
                                        , cashes
                                        , impacts
                                        )
                                      ] 
              EdgeReverseContext _ -> [] 
          |
            (_, v) <- M.toList edgeContexts
          ]
    logInfo ""
    logInfo $ "Optimizing and checking solution for years " ++ show years ++ " . . ."
    let
      (existingSupply, totalDemand) =
        ((sum . concat) *** (sum . concat))
          $ unzip
          [
            case k of
              DemandEdge   _ -> ([]        , capacity v)
              ExistingEdge _ -> (capacity v, []        )
              _              -> ([]        , []        )
          |
            (k, v) <- M.toList edgeContexts
          ]
      totalCost =
        sum
          $ concat
          [
            case v of
              EdgeContext{..}      -> (fSale <:) <$> flows
              EdgeReverseContext{} -> []
          |
            (_, v) <- M.toList edgeContexts
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
                                              logError $ "Unsatisfied demand at \"" ++ show location ++ "\" of " ++ show (capacity v .-. (abs <$> reserved v)) ++ " kg/yr."
                                              return True
                                       else do
                                              logDebug $ "Satisfied demand at \"" ++ show location ++ "\" of " ++ show (reserved v) ++ " kg/yr."
                                              return False
            ExistingEdge location -> do
                                       when (reserved v #|<# capacity v)
                                         . logDebug $ "Underused existing production at \"" ++ show location ++ "\" of " ++ show (capacity v .-. (abs <$> reserved v)) ++ " kg/yr."
                                       return False
            _                     -> return False
        |
          (k, v) <- M.toList edgeContexts
        ]
    logInfo $ " . . . checks complete."
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
