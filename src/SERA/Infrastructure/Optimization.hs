{-# LANGUAGE DataKinds #-}
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
, optimize
) where


import Control.Monad (guard)
import Data.Daft.DataCube (evaluable, evaluate, knownKeys)
import Data.Daft.Vinyl.FieldCube ((!), toKnownRecords, σ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Default.Util (inf, nan)
import Data.Graph.Algorithms (minimumCostFlow)
import Data.Graph.Types (Capacity(..), Graph(..), makeGraph)
import Data.List (find, nub, unzip4)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Sum(..))
import Data.Tuple.Util (fst3)
import Debug.Trace (trace)
import SERA.Infrastructure.Optimization.Legacy (Optimum(..))
import SERA.Infrastructure.Types (Cash, Construction, fConsumption, CostCategory(..), fCostCategory, DemandCube, Flow, fFlow, Impact, fLoss, fProduction, fSalvage)
import SERA.Material.Prices (localize)
import SERA.Material.Types (IntensityCube, fMaterial, fPrice, PriceCube, Pricer)
import SERA.Network.Types (fFrom, Infrastructure(..), fInfrastructure, fLength, Location, FLocation, fLocation, fSale, fTo, Network(..))
import SERA.Process.Reification.Technology (TechnologyOperation, technologyReifier)
import SERA.Process.Types (fCapacity, fCapitalCost, fCost, fDutyCycle, fExtended, fFixedCost, fLifetime, fNameplate, Pathway(..), fPathway, ProcessLibrary(..), Productive(..), fProductive, fStage, Technology(..), fTechnology, isProduction, fTransmission, fVariableCost)
import SERA.Types (Year, fYear)

import qualified Data.Map as M
import qualified Data.Set as S


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
                    productive = fProductive <: node
              , productive == Central || productive == Yes
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


type TechnologyBuilder = Int -> Year -> Double {- Demand -} -> TechnologyContext


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
    , nameplate  :: Double
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
                                                           , capacity   = maybe 0 (fConsumption <:) $ demandCube `evaluate` (fLocation =: location <+> fYear =: year)
                                                           , nameplate  = 0
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
                                                             , nameplate  = fCapacity <: existing
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
                                                                                                  <+> fProduction     =: flow
                                                                                                  <+> fFlow           =: 0
                                                                                                  <+> fLoss           =: 0
                                                                                                  <+> fSale           =: flow * fCost <: existing
                                                                                                  <+> fSalvage        =: 0
                                                                                                , [
                                                                                                      fInfrastructure =: infrastructure
                                                                                                    <+> fYear         =: year'
                                                                                                    <+> fCostCategory =: Variable
                                                                                                    <+> fSale         =: flow * fCost <: existing
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
                                                                              uncurry TechnologyContext
                                                                              . fromJust
                                                                              $ technologyReifier
                                                                                  processLibrary
                                                                                  (localize intensityCube location)
                                                                                  (makePricer priceCube location)
                                                                                  (fInfrastructure =: Infrastructure (show identifier ++ "/" ++ show year' ++ "/" ++ show i) <+> fLocation =: location)
                                                                                  year'
                                                                                  demand
                                                                                  0
                                                                                  technology
                                                           , capacity   = inf
                                                           , nameplate  = 0
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
                                                                              uncurry TechnologyContext
                                                                              . fromJust
                                                                              $ technologyReifier
                                                                                  processLibrary
                                                                                  (localize intensityCube location)
                                                                                  (makePricer priceCube location)
                                                                                  (fInfrastructure =: Infrastructure (show identifier ++ "/" ++ show year' ++ "/" ++ show i) <+> fLocation =: location)
                                                                                  year'
                                                                                  demand
                                                                                  0
                                                                                  technology
                                                           , capacity   = inf
                                                           , nameplate  = 0
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
                                                                                uncurry TechnologyContext
                                                                                . fromJust
                                                                                $ technologyReifier
                                                                                    processLibrary
                                                                                    (localize intensityCube location)
                                                                                    (makePricer priceCube from)
                                                                                    (fInfrastructure =: Infrastructure (show identifier ++ "/" ++ show year' ++ "/" ++ show i) <+> fLocation =: location)
                                                                                    year'
                                                                                    demand
                                                                                    distance
                                                                                    technology
                                                             , capacity   = inf
                                                             , nameplate  = 0
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
        . fmap (flip ($ year) (abs $ reserved + delta) . operation) -- FIXME: Check for negative flows everywhere.
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
    (newCost - oldCost) / abs delta


adjustEdge :: Year -> Double -> EdgeContext -> EdgeContext
adjustEdge year delta edgeContext@EdgeContext{..} =
  let
    edgeContext' =
      if abs (delta + reserved) <= nameplate -- FIXME: Rewrite this.
        then edgeContext
             {
               reserved = reserved + delta
             }
        else let
               flow = abs (delta + reserved)
               adjustable'@TechnologyContext{..} = (fromJust builder) (length fixed + 1) year flow
             in
               edgeContext
               {
                 nameplate = fNameplate <: construction * fDutyCycle <: construction
               , reserved = reserved + delta
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
      capacity' =
        case edgeContext of
          EdgeContext{..} -> capacity
          _               -> nan
    guard
      $ capacity' > 0
    return
      (
        case edge of
          DemandEdge _                              -> Sum 0
          ExistingEdge _                            -> Sum . (fVariableCost <:) . construction . head $ fixed edgeContext
          PathwayReverseEdge location pathway stage -> fst . fromJust . costFunction year (Capacity (- flow)) context $ PathwayForwardEdge location pathway stage
          _                                         -> Sum $ marginalCost year flow edgeContext
      , context
      )
costFunction _ _ _ _ = error "costFunction: no flow."


capacityFunction :: NetworkContext -> Edge -> Maybe (Capacity Double, NetworkContext)
capacityFunction context edge =
  do
    let
      edgeContext = edgeContexts context M.! edge
      capacity' =
        case edgeContext of
          EdgeContext{..} -> capacity
          _               -> nan
    guard
      $ capacity' > 0
    return (Capacity capacity', context)


flowFunction :: Year -> Capacity Double -> NetworkContext -> Edge -> Maybe NetworkContext
flowFunction year (Capacity flow) context edge =
  do
    let
      edgeContext = edgeContexts context M.! edge
      update edgeContext' = context { edgeContexts = M.insert edge edgeContext' $ edgeContexts context }
      capacity' =
        case edgeContext of
          EdgeContext{..} -> capacity
          _               -> nan
    guard
      $ capacity' > 0
    return
      $ case edge of
        DemandEdge _                              -> update $ edgeContext { capacity = capacity' - flow }
        ExistingEdge _                            -> update $ edgeContext { capacity = capacity' - flow }
        PathwayReverseEdge location pathway stage -> fromJust . flowFunction year (Capacity (- flow)) context $ PathwayForwardEdge location pathway stage
        _                                         -> update $ adjustEdge year flow edgeContext
flowFunction _ _ _ _ = error "flowFunction: no flow."


optimize :: Year -> Network -> DemandCube -> IntensityCube '[FLocation] -> ProcessLibrary -> PriceCube '[FLocation] -> Optimum
optimize year' network demandCube intensityCube processLibrary priceCube =
  let
    graph = networkGraph network demandCube processLibrary
    context = buildContext graph network processLibrary intensityCube priceCube demandCube year'
    NetworkContext{..} =
      minimumCostFlow
        (costFunction year')
        capacityFunction
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
  in
    Optimum
      (concat constructions')
      (concat flows')
      (concat cashes')
      (concat impacts')
