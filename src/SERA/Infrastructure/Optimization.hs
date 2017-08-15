{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}


module SERA.Infrastructure.Optimization
-- FIXME
where


import Control.Applicative (liftA2)
import Control.Arrow ((***), first)
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Default.Util (inf)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, isJust)
import Data.Monoid (Sum(..), (<>))
import Data.Set (Set, toList)
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Vinyl.Lens (type (∈), rput)
import SERA (unsafePrint)
import SERA.Infrastructure.Types
import SERA.Material.Prices
import SERA.Material.Types
import SERA.Network.Types
import SERA.Process.Reification.Pathway
import SERA.Process.Reification.Technology (TechnologyOperation, technologyReifier)
import SERA.Process.Types hiding (deliveries)
import SERA.Types
import SERA.Types.TH (makeField)


import qualified Data.Map.Strict as M ((!), elems, empty, filter, filterWithKey, findMin, fromList, map, member, null, toList, union, unionWith)


type DemandCube' = '[FLocation, FYear] *↝ '[FConsumption, FArea]


data GlobalContext =
  GlobalContext
  {
    priceCube          :: PriceCube '[FLocation]
  , processLibrary     :: ProcessLibrary
  , intensityCube      :: IntensityCube '[FLocation]
  , network            :: Network
  , demandCube         :: DemandCube'
  , firstYear          :: Year
  , lastYear           :: Year
  , timeWindow         :: Year
  , discountRate       :: Double
  , escalationRate     :: Double
  , interpolate        :: Bool
  , maximumPathLength  :: Maybe Double
  , localPenaltyFactor :: Maybe Double
  , extantConstruction :: [Construction]
  , extantFlow         :: [Flow]
  , extantCash         :: [Cash]
  , extantImpact       :: [Impact]
  }

  
type TechnologyReifier = Technology -> Maybe (Construction, TechnologyOperation)


type DeliveryReifier = Pathway -> Maybe ([Construction], PathwayOperation)


productionReifier :: PriceCube '[FLocation] -> ProcessLibrary -> IntensityCube '[FLocation] -> Location -> Double -> Year -> Double -> TechnologyReifier
productionReifier priceCube processLibrary intensityCube loc distance year consumption =
  technologyReifier
    processLibrary
    (localize intensityCube loc)
    (\m y -> maybe 0 (fPrice <:) $ priceCube `evaluate` (fMaterial =: m <+> fYear =: y <+> fLocation =: loc)) -- FIXME: extrapolate
    (fInfrastructure =: Infrastructure (location loc ++ " @ " ++ show year) <+> fLocation =: loc)
    year
    consumption
    distance


deliveryReifier' :: PriceCube '[FLocation] -> ProcessLibrary -> IntensityCube '[FLocation] -> Location -> Double -> Year -> Double -> DeliveryReifier
deliveryReifier' priceCube processLibrary intensityCube loc distance year consumption =
  pathwayReifier
    (\_ _ -> True)
    processLibrary
    (
      technologyReifier
        processLibrary
        (localize intensityCube loc)
        (\m y -> maybe 0 (fPrice <:) $ priceCube `evaluate` (fMaterial =: m <+> fYear =: y <+> fLocation =: loc)) -- FIXME: extrapolate
    )
    year
    consumption
    distance
    (Infrastructure (location loc ++ " @ " ++ show year), GenericPath loc [(loc, 0)] loc)


productionCandidates :: TechnologyReifier -> [Technology] -> [(Technology, ([Construction], PathwayOperation))]
productionCandidates reifyTechnology techs =
  catMaybes
    [
      do
        (construction, operate) <- reifyTechnology tech
        return (tech, ([construction], \y c -> let (f, cs, is) = operate y c in ([f], cs, is)))
    |
      tech <- techs
    ]


deliveryCandidates :: DeliveryReifier -> [Pathway] -> [(Pathway, ([Construction], PathwayOperation))]
deliveryCandidates reifyDelivery paths' =
  catMaybes
    [
      do
        (construction, operate) <- reifyDelivery path
        return (path, (construction, operate))
    |
      path <- paths'
    ]


costCandidate :: Year -> ([Construction], PathwayOperation) -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> (Sum Double, Optimum)
costCandidate year (construction, operate) demands =
  let
    (flows, cashes, impacts) = unzip3 $ (\rec -> operate (fYear <: rec) (fConsumption <: rec)) <$> demands
  in
    (
      Sum
        $ sum
        $ (\rec -> fSale <: rec - (if fYear <: rec == year then fSalvage <: rec else 0))
        <$> filter (\rec -> fYear <: rec <= year)
        (concat flows)
    , Optimum construction (concat flows) (concat cashes) (concat impacts)
    )


costedCandidates :: Ord a
                 => Year
                 -> ([a] -> [(a, ([Construction], PathwayOperation))])
                 -> Set a
                 -> [FieldRec '[FLocation, FYear, FConsumption, FArea]]
                 -> Map a (Sum Double, Optimum)
costedCandidates year makeCandidates xs demands =
  M.fromList
    [
      (x, costCandidate year candidate demands)
    |
      (x, candidate) <- makeCandidates $ toList xs
    ]


characterizeDemands :: Year -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> (Location, Double, Year, Double)
characterizeDemands year' demands =
  let
    loc = fLocation <: head demands :: Location
    area = fArea <: head demands
    (year, consumption) =
      (minimum *** maximum)
        $ unzip
        [
          (fYear <: rec, fConsumption <: rec)
        |
          rec <- demands
        , fYear <: rec <= year'
        , 0 < fConsumption <: rec
        ]
  in
    (loc, 2 * sqrt area, year, consumption)


data Optimum =
  Optimum
  {
    optimalConstruction :: [Construction]
  , optimalFlow         :: [Flow]
  , optimalCash         :: [Cash]
  , optimalImpact       :: [Impact]
  }
    deriving (Eq, Ord, Show)

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

$(makeField "Optimum" "Optimum" ''Optimum)


doSalvage :: Year -> Optimum -> Optimum
doSalvage year optimum@Optimum{..} =
  let
    salvages =
      [
            fInfrastructure =: fInfrastructure <: rec
        <+> fYear           =: fYear           <: rec
        <+> fCostCategory   =: Salvage
        <+> fSale           =: - salvage
      |
        rec <- optimalFlow
      , fYear <: rec == year
      , let salvage = fSalvage <: rec
      , salvage > 0
      ]
  in
    optimum
    {
      optimalFlow = [
                      if fYear <: rec == year
                        then rput (Field $ fSale <:rec - fSalvage <: rec :: ElField FSale) rec
                        else rec
                    |
                      rec <- optimalFlow
                    ]
    , optimalCash = optimalCash ++ salvages
    }


optimize :: GlobalContext -> Year -> ('[FLocation] *↝ '[FConsumption], Optimum)
optimize globalContext@GlobalContext{..} year =
  let
    allYears :: Set (FieldRec '[FYear])
    allYears = undefined
    filterYear :: (FYear ∈ ks) => FieldRec ks -> Bool
    filterYear rec = fYear <: rec >= year && fYear <: rec < year + timeWindow
    supply =
      κ' allYears (\recs -> fConsumption =: maximum ((fConsumption <:) <$> recs))
        $ σ (const . filterYear) demandCube
    globalContext' =
      globalContext
      {
        demandCube =
          π (\_ rec -> fConsumption =: minimum [fConsumption <: rec, fProduction <: rec] <+> fArea =: fArea <: rec)
            $ demandCube ⋈ π (\_ rec -> fProduction =: fConsumption <: rec) supply
      }
    locations =
      sortBy (flip compare)
        [
          (fConsumption <: rec, fLocation <: rec)
        |
          rec <- toKnownRecords supply
        ]
    f :: Map Location CostedOptimum -> (Double, Location) -> Map Location CostedOptimum
    f previous loc =
      (maybe id (\s -> M.map (first (Sum s *))) localPenaltyFactor) $
        M.fromList (
          cheapestLocally
            $ toLocalContext globalContext' year $ snd loc
        ) `M.union` previous
    singly = foldl f M.empty locations
    g :: Map Location CostedOptimum -> ((Double, Location), (Double, Location)) -> Map Location CostedOptimum
    g previous (locSource, locSink) =
      M.fromList (
        let
          previousSource = previous M.! snd locSource
          previousSink   = previous M.! snd locSink
          previousCost = fst $ previousSource <> previousSink
          capacitySource = sum $ fmap (fCapacity <:) $ filter ((/= No) . (fProductive <:)) $ optimalConstruction $ snd $ previousSource
          capacitySink   = sum $ fmap (fCapacity <:) $ filter ((/= No) . (fProductive <:)) $ optimalConstruction $ snd $ previousSink
          revisions =
            cheapestRemotely
              globalContext'
              (capacitySource, toLocalContext globalContext' year $ snd locSource)
              (capacitySink  , toLocalContext globalContext' year $ snd locSink  )
          revisedCost  = mconcat $ fst . snd <$> revisions
        in
          if locSource <= locSink || capacitySource <= 0 || capacitySink <= 0 || null revisions || previousCost < revisedCost
            then []
            else revisions
      ) `M.union` previous
    doubly = foldl g (unsafePrint " . . . identifying regional synergies . . ." singly) $ liftA2 (,) locations locations
  in
    (
      supply
    , doSalvage lastYear $ mconcat $ fmap snd $ M.elems doubly
    )


data LocalContext =
  LocalContext
  {
    localLocation      :: Location
  , localDistance      :: Double
  , localYears         :: [Year]
  , localDemands       :: Map Year Double
  , productionsOnsite  :: Map Technology CostedOptimum
  , productionsCentral :: Map Technology CostedOptimum
  , deliveries         :: Map Pathway CostedOptimum
  }


type CostedOptimum = (Sum Double, Optimum)


toLocalContext :: GlobalContext -> Year -> Location -> LocalContext
toLocalContext GlobalContext{..} year' localLocation =
  let
    localYears = [year' .. year'+timeWindow-1]
    filterLocally :: (FYear ∈ ks, FLocation ∈ ks) => FieldRec ks -> Bool
    filterLocally rec = fYear <: rec >= head localYears && fLocation <: rec == localLocation
    demandCube' = σ (const . filterLocally) demandCube
    demands' = toKnownRecords demandCube'
    localDemands = M.fromList [(fYear <: rec, fConsumption <: rec) | rec <- demands']
    (_, localDistance, year, consumption) = characterizeDemands (last localYears) demands'
    reifyTechnology = productionReifier priceCube processLibrary intensityCube localLocation localDistance year
    reifyDelivery = deliveryReifier' priceCube processLibrary intensityCube localLocation localDistance year 
    productionsOnsite =
      costedCandidates
        (last localYears)
        (productionCandidates $ reifyTechnology consumption)
        (productions' Onsite processLibrary)
        demands'
    productionsCentral =
      costedCandidates
        (last localYears)
        (productionCandidates $ reifyTechnology consumption)
        (productions' Central processLibrary)
        demands'
    deliveries =
      costedCandidates
        (last localYears)
        (deliveryCandidates $ reifyDelivery consumption)
        (localPathways processLibrary)
        demands'
  in
    LocalContext{..}


toLocalContext' :: GlobalContext -> LocalContext -> Double -> Map Year Double -> LocalContext
toLocalContext' GlobalContext{..} previous@LocalContext{..} minimumCapacity demandIncrement {- FIXME -} =
  let
    localDemands' = M.unionWith (+) localDemands demandIncrement
    year = fst $ M.findMin $ M.filter (> 0) localDemands'
    consumption = maximum $ minimumCapacity : M.elems (M.filterWithKey (\k _ -> k <= last localYears) localDemands')
    demands' =
      [
            fLocation    =: localLocation
        <+> fYear        =: y
        <+> fConsumption =: c
        <+> fArea        =: (localDistance / 2)^(2::Int)
      |
        (y, c) <- M.toList localDemands'
      ]
    reifyTechnology = productionReifier priceCube processLibrary intensityCube localLocation localDistance year
  in
    previous
    {
      localDemands = localDemands'
    , productionsCentral =
        costedCandidates
          (last localYears)
          (productionCandidates $ reifyTechnology consumption)
          (productions' Central processLibrary)
          demands'
    }


noCandidate :: CostedOptimum
noCandidate = (Sum inf, mempty)


cheapestLocally :: LocalContext -> [(Location, CostedOptimum)]
cheapestLocally LocalContext{..} =
  let
    candidatesOnsite = noCandidate : M.elems productionsOnsite
    candidatesCentral = noCandidate : M.elems productionsCentral
    candidatesLocal = noCandidate : M.elems deliveries
    best = minimum [minimum candidatesOnsite, minimum candidatesCentral <> minimum candidatesLocal]
  in
    if best == noCandidate
      then error ("No eligible technologies in year " ++ show (maximum localYears) ++ ".") -- FIXME: Move to error monad.
      else [(localLocation, best)]


costedDeliveryCandidates :: GlobalContext -> LocalContext -> LocalContext -> Map Pathway CostedOptimum
costedDeliveryCandidates GlobalContext{..} localContextSource localContextSink =
  let
    year' = fst $ M.findMin $ M.filter (> 0) $ localDemands localContextSink
    consumption' = maximum $ M.elems $ M.filterWithKey (\k _ -> k <= last (localYears localContextSink)) $ localDemands localContextSink
    Network{..} = network
    path = paths M.! (localLocation localContextSource, localLocation localContextSink)
    transmissionReifier' =
      pathwayReifier
        (\_ _ -> True)
        processLibrary
        (
          technologyReifier
            processLibrary
            (localize intensityCube $ localLocation localContextSink) -- FIXME
            (\m y -> maybe 0 (fPrice <:) $ priceCube `evaluate` (fMaterial =: m <+> fYear =: y <+> fLocation =: localLocation localContextSink)) -- FIXME: extrapolate
        )
        year'
        consumption'
        (localDistance localContextSink)
        (Infrastructure (location (localLocation localContextSink) ++ " @ " ++ show year'), path)
  in
    M.fromList
      [
        let
          (flows, cashes, impacts) = unzip3 $ map (uncurry operate) $ M.toList $ localDemands localContextSink
          year = last $ localYears localContextSink
        in
          (
            pathway
          , (
              Sum
                $ sum
                $ (\rec -> fSale <: rec - (if fYear <: rec == year then fSalvage <: rec else 0))
                <$> filter (\rec -> fYear <: rec <= year)
                (concat flows)
            , Optimum construction (concat flows) (concat cashes) (concat impacts)
            )
          )
      |
        pathway <- toList $ transmissionPathways processLibrary
      , let z = transmissionReifier' pathway
      , isJust z
      , let Just (construction, operate) = z
      ]


cheapestRemotely :: GlobalContext -> (Double, LocalContext) -> (Double, LocalContext) -> [(Location, CostedOptimum)]
cheapestRemotely globalContext (productionSource, localContextSource) (_, localContextSink) =
  let
    ps = productionsCentral $ toLocalContext' globalContext localContextSource productionSource $ localDemands localContextSink
    bestSource = minimum ps
    bestLocal = minimum $ M.elems $ deliveries localContextSource
    bestSink = minimum $ M.elems $ costedDeliveryCandidates globalContext localContextSource localContextSink
  in
    if not (M.null ps) && (localLocation localContextSource, localLocation localContextSink) `M.member` (paths $ network globalContext)
      then [
             (localLocation localContextSource, bestSource <> bestLocal)
           , (localLocation localContextSink  , bestSink  )
           ]
      else []
