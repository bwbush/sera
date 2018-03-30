{-# LANGUAGE DataKinds #-}
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
, Optimum(..)
, optimize
) where


import Control.Arrow ((&&&), (***))
import Control.Monad (guard, when)
import Control.Monad.Log (logDebug, logError, logInfo, logNotice)
import Data.Daft.DataCube (evaluable, evaluate, knownKeys)
import Data.Daft.Vinyl.FieldCube ((!), toKnownRecords, σ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Default.Util (inf)
import Data.Graph.Algorithms (minimumCostFlow)
import Data.Graph.Types (Capacity(..), Graph(..), makeGraph)
import Data.List (find, nub, unzip4)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Sum(..), (<>))
import Data.Tuple.Util (fst3)
import SERA (SeraLog)
import SERA.Material (Pricer, localize)
import SERA.Network (Network(..))
import SERA.Process.Reification (TechnologyOperation, technologyReifier)
import SERA.Process (ProcessLibrary(..), isProduction)
import SERA.Types.Cubes (DemandCube, IntensityCube, PriceCube)
import SERA.Types.Fields (CostCategory(..), fCapacity, fCapitalCost, fCost, fDutyCycle, fExtended, fFixedCost, fFrom, Infrastructure(..), fInfrastructure, fLifetime, fNameplate, fCostCategory, fFlow, fFuelConsumption, fLength, Location, FLocation, fLocation, fLoss, fMaterial, fNonFuelConsumption, fPrice, fProduction, fSale, fSalvage, Pathway(..), fPathway, Productive(..), fProductive, fStage, Technology(..), fTechnology, fTo, fTransmission, fVariableCost, Year, fYear)
import SERA.Types.Records (Cash, Construction, Flow, Impact)

import qualified Data.Map as M
import qualified Data.Set as S


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


type TechnologyBuilder = Int -> Year -> Double {- Demand -} -> Maybe TechnologyContext


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
    , capacity   :: Double
    , reserved   :: Double
    , fixed      :: [TechnologyContext]
    , adjustable :: Maybe TechnologyContext
    , flows      :: [Flow]
    , cashes     :: [Cash]
    , impacts    :: [Impact]
    }
  | EdgeReverseContext Edge


data NetworkContext =
  NetworkContext
  {
    edgeContexts :: Map Edge EdgeContext
  }


buildContext :: NetworkGraph -> Network -> ProcessLibrary -> IntensityCube '[FLocation] -> PriceCube '[FLocation] -> DemandCube -> Year -> NetworkContext
buildContext Graph{..} Network{..} processLibrary@ProcessLibrary{..} intensityCube priceCube demandCube year =
  let
    edgeContexts =
      M.fromList
        [
          (
            edge
          , case edge of
              DemandEdge location                       -> EdgeContext
                                                           {
                                                             builder    = Nothing
                                                           , capacity   = maybe 0 (\rec -> fFuelConsumption <: rec + fNonFuelConsumption <: rec) $ demandCube `evaluate` (fLocation =: location <+> fYear =: year)
                                                           , reserved   = 0
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
                                                           }
              ExistingEdge infrastructure               -> let
                                                             existing = existingCube ! (fInfrastructure =: infrastructure)
                                                           in
                                                             EdgeContext
                                                             {
                                                               builder    = Nothing
                                                             , capacity   = fCapacity <: existing
                                                             , reserved   = 0
                                                             , fixed      = [
                                                                                TechnologyContext
                                                                                {
                                                                                  construction =     fInfrastructure =: infrastructure
                                                                                                 <+> fLocation       =: fLocation <: existing
                                                                                                 <+> fTechnology     =: Technology "Existing or Planned"
                                                                                                 <+> fProductive     =: Central
                                                                                                 <+> fYear           =: fYear <: existing
                                                                                                 <+> fLifetime       =: 1000
                                                                                                 <+> fNameplate      =: fCapacity <: existing
                                                                                                 <+> fDutyCycle      =: 1
                                                                                                 <+> fLength         =: 0
                                                                                                 <+> fCapitalCost    =: 0
                                                                                                 <+> fFixedCost      =: 0
                                                                                                 <+> fVariableCost   =: fCost <: existing
                                                                                , operation = \year' flow ->
                                                                                                (
                                                                                                      fInfrastructure =: infrastructure
                                                                                                  <+> fYear           =: year'
                                                                                                  <+> fTechnology     =: Technology "Existing or Planned"
                                                                                                  <+> fProduction     =: flow
                                                                                                  <+> fFlow           =: 0
                                                                                                  <+> fLoss           =: 0
                                                                                                  <+> fSale           =: abs flow * fCost <: existing
                                                                                                  <+> fSalvage        =: 0
                                                                                                , [
                                                                                                      fInfrastructure =: infrastructure
                                                                                                    <+> fYear         =: year'
                                                                                                    <+> fCostCategory =: Variable
                                                                                                    <+> fSale         =: abs flow * fCost <: existing
                                                                                                  ]
                                                                                                , []
                                                                                                , flow
                                                                                                )
                                                                                }
                                                                               ]
                                                             , adjustable = Nothing
                                                             , flows      = []
                                                             , cashes     = []
                                                             , impacts    = []
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
                                                                                  (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show year' ++ "-" ++ show i) <+> fLocation =: location)
                                                                                  year'
                                                                                  demand
                                                                                  0
                                                                                  technology
                                                           , capacity   = 0
                                                           , reserved   = 0
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
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
                                                                                  (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show year' ++ "-" ++ show i) <+> fLocation =: location)
                                                                                  year'
                                                                                  demand
                                                                                  0
                                                                                  technology
                                                           , capacity   = 0
                                                           , reserved   = 0
                                                           , fixed      = []
                                                           , adjustable = Nothing
                                                           , flows      = []
                                                           , cashes     = []
                                                           , impacts    = []
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
                                                                                    (fInfrastructure =: Infrastructure ("INFR-" ++ show identifier ++ "-" ++ show year' ++ "-" ++ show i) <+> fLocation =: location)
                                                                                    year'
                                                                                    demand
                                                                                    distance
                                                                                    technology
                                                             , capacity   = 0
                                                             , reserved   = 0
                                                             , fixed      = []
                                                             , adjustable = Nothing
                                                             , flows      = []
                                                             , cashes     = []
                                                             , impacts    = []
                                                             }
              PathwayReverseEdge location pathway stage -> EdgeReverseContext $ PathwayForwardEdge location pathway stage
          )
        |
          (identifier, edge) <- zip [(1::Int)..] $ S.toList allEdges
        ]
  in
    NetworkContext{..}


costEdge :: Year -> Double -> EdgeContext -> Double
costEdge year delta =
  sum
    . fmap (fSale <:)
    . fst3
    . costEdge' year delta -- FIXME: Handle salvage.


costEdge' :: Year -> Double -> EdgeContext -> ([Flow], [Cash], [Impact])
costEdge' year delta EdgeContext{..} =
  let
    (flows', cashes', impacts', _) =
      unzip4
        . fmap (flip ($ year) (reserved + delta) . operation) -- FIXME: Check for negative flows everywhere.
        $ (maybe id (:) adjustable) fixed
  in
    (flows', concat cashes', concat impacts')
costEdge' _ _ _ = error "costEdge': edge is reversed."


marginalCost :: Year -> Double -> EdgeContext -> Double
marginalCost year delta edgeContext =
  let
    oldCost = costEdge year 0                         edgeContext
    newCost = costEdge year 0 $ adjustEdge year delta edgeContext
  in
    if False
      then (newCost - oldCost) / abs delta
      else newCost / abs (reserved edgeContext + delta)


adjustEdge :: Year -> Double -> EdgeContext -> EdgeContext
adjustEdge year delta edgeContext@EdgeContext{..} =
  let
    reserved' = reserved + delta
    edgeContext' =
      if abs reserved' <= capacity -- FIXME: Rewrite this.
        then edgeContext
             {
               reserved = reserved'
             }
        else let
               adjustable'@TechnologyContext{..} = fromJust $ (fromJust builder) (length fixed + 1) year reserved'
             in
               edgeContext
               {
                 capacity = fNameplate <: construction * fDutyCycle <: construction
               , reserved = reserved'
               , adjustable = Just adjustable'
               }
    (flows', cashes', impacts') = costEdge' year 0 edgeContext'
  in
    edgeContext'
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


costFunction :: Year -> Capacity Double -> NetworkContext -> Edge -> Maybe (Sum Double, NetworkContext)
costFunction year (Capacity flow) context edge =
  do
    let
      edgeContext = edgeContexts context M.! edge
      (capacity', builder') =
        case edgeContext of
          EdgeContext{..}          -> (capacity - abs reserved, builder)
          EdgeReverseContext edge' -> (\z -> capacity z - abs (reserved z)) &&& builder $ edgeContexts context M.! edge'
      canBuild =  maybe False (\b -> isJust $ b 0 year 0) builder'
    guard
      $ capacity' > 0 || canBuild
    return
      (
        case edge of
          DemandEdge _                              -> Sum 0
          ExistingEdge _                            -> Sum . sum $ (fVariableCost <:) . construction <$> fixed edgeContext
          PathwayReverseEdge location pathway stage -> fst . fromJust . costFunction year (Capacity (- flow)) context $ PathwayForwardEdge location pathway stage
          _                                         -> Sum $ marginalCost year flow edgeContext
      , context
      )
costFunction _ _ _ _ = Nothing -- error "costFunction: no flow."


capacityFunction :: Year -> NetworkContext -> Edge -> Maybe (Capacity Double, NetworkContext)
capacityFunction year context edge =
  case edgeContexts context M.! edge of
    EdgeContext{..}          -> do
                                  let
                                    canBuild =  maybe False (\b -> isJust $ b 0 year 0) builder
                                  guard
                                    $ capacity > abs reserved || canBuild
                                  return
                                    (
                                      if canBuild
                                        then Capacity inf
                                        else Capacity $ capacity - abs reserved
                                    , context
                                    )
    EdgeReverseContext edge' -> capacityFunction year context edge'


flowFunction :: Year -> Capacity Double -> NetworkContext -> Edge -> Maybe NetworkContext
flowFunction year (Capacity flow) context edge =
  do
    let
      edgeContext = edgeContexts context M.! edge
      update edgeContext' = context { edgeContexts = M.insert edge edgeContext' $ edgeContexts context }
      (capacity', builder') =
        case edgeContext of
          EdgeContext{..}          -> (capacity - abs reserved, builder)
          EdgeReverseContext edge' -> (\z -> capacity z - abs (reserved z)) &&& builder $ edgeContexts context M.! edge'
      canBuild =  maybe False (\b -> isJust $ b 0 year 0) builder'
    guard
      $ capacity' > 0 || canBuild
    return
      $ case edge of
        DemandEdge _                              -> update $ edgeContext { reserved = reserved edgeContext + flow}
        ExistingEdge _                            -> update $ let
                                                                (flow', cashes', impacts', _) = operation (head $ fixed edgeContext) year $ reserved edgeContext + flow -- FIXME: remove head.
                                                              in
                                                                edgeContext
                                                                {
                                                                  reserved = reserved edgeContext + flow
                                                                , flows    = [flow']
                                                                , cashes   = cashes'
                                                                , impacts  = impacts'
                                                                }
        PathwayReverseEdge location pathway stage -> fromJust . flowFunction year (Capacity (- flow)) context $ PathwayForwardEdge location pathway stage
        _                                         -> update $ adjustEdge year flow edgeContext
flowFunction _ _ _ _ = error "flowFunction: no flow."


optimize :: SeraLog m => Year -> Network -> DemandCube -> IntensityCube '[FLocation] -> ProcessLibrary -> PriceCube '[FLocation] -> m Optimum
optimize year' network demandCube intensityCube processLibrary priceCube =
  do
    let
      graph = networkGraph network demandCube processLibrary
      context = buildContext graph network processLibrary intensityCube priceCube demandCube year'
      NetworkContext{..} =
        minimumCostFlow
          (costFunction year')
          (capacityFunction year')
          (flowFunction year')
          graph
          context
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
    logInfo $ "Optimizing and checking solution for year " ++ show year' ++ " . . ."
    let
      (existingSupply, totalDemand) =
        (sum *** sum)
          $ unzip
          [
            case k of
              DemandEdge   _ -> (0         , capacity v)
              ExistingEdge _ -> (capacity v, 0         )
              _              -> (0         , 0         )
          |
            (k, v) <- M.toList edgeContexts
          ]
    logNotice $ "Total existing production capacity: " ++ show existingSupply ++ " kg/yr."
    logNotice $ "Total demand: "                       ++ show totalDemand    ++ " kg/yr."
    sequence_
      [
        case k of
          DemandEdge   location -> when (capacity v > reserved v)
                                     . logError $ "Unsatisfied demand at \"" ++ show location ++ "\" of " ++ show (capacity v - reserved v) ++ " kg/yr."
          ExistingEdge location -> when (capacity v > reserved v)
                                     . logDebug $ "Underused existing production at \"" ++ show location ++ "\" of " ++ show (capacity v - reserved v) ++ " kg/yr."
          _                     -> return ()
      |
        (k, v) <- M.toList edgeContexts
      ]
    logInfo $ " . . . checks complete."
    return
      $ Optimum
        (concat constructions')
        (concat flows')
        (concat cashes')
        (concat impacts')
