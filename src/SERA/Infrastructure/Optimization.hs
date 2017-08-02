{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}


module SERA.Infrastructure.Optimization
-- FIXME
where


import Control.Arrow ((***))
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, isJust)
import Data.Monoid (Sum(..), (<>))
import Data.Set (Set, toList)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import SERA (trace')
import SERA.Infrastructure.Types
import SERA.Material.Prices
import SERA.Material.Types
import SERA.Network.Types
import SERA.Process.Reification.Pathway
import SERA.Process.Reification.Technology (TechnologyOperation, technologyReifier)
import SERA.Process.Types hiding (deliveries)
import SERA.Types
import SERA.Types.TH (makeField)


import qualified Data.Map.Strict as M ((!), elems, empty, filter, findMin, fromList, toList, union)


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
    (Infrastructure (location loc ++ " @ " ++ show year), GenericPath loc [(loc, distance)] loc)


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


costCandidate :: ([Construction], PathwayOperation) -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> (Sum Double, Optimum)
costCandidate (construction, operate) demands =
  let
    (flows, cashes, impacts) = unzip3 $ (\rec -> operate (fYear <: rec) (fConsumption <: rec)) <$> demands
  in
    (
      Sum $ sum $ (fSale <:) <$> concat flows
    , Optimum construction (concat flows) (concat cashes) (concat impacts)
    )


costedCandidates :: Ord a
                 => ([a] -> [(a, ([Construction], PathwayOperation))])
                 -> Set a
                 -> [FieldRec '[FLocation, FYear, FConsumption, FArea]]
                 -> Map a (Sum Double, Optimum)
costedCandidates makeCandidates xs demands =
  M.fromList
    [
      (x, costCandidate candidate demands)
    |
      (x, candidate) <- makeCandidates $ toList xs
    ]


characterizeDemands :: [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> (Location, Double, Year, Double)
characterizeDemands demands =
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
    locations =
      fmap snd
        $ sortBy (flip compare)
        [
          (fConsumption <: rec, fLocation <: rec)
        |
          rec <- toKnownRecords supply
        ]
    f :: Map Location Optimum -> Location -> Map Location Optimum
    f previous loc =
      (
        M.fromList
          $ cheapestLocally
          $ toLocalContext globalContext year loc
      ) `M.union` previous
    singly = foldl f M.empty locations
  in
    (
      supply
    , mconcat
        $ M.elems singly
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
    filterLocally rec = fYear <: rec `elem` localYears && fLocation <: rec == localLocation
    demandCube' = σ (const . filterLocally) demandCube
    demands' = toKnownRecords demandCube'
    localDemands = M.fromList [(fYear <: rec, fConsumption <: rec) | rec <- demands']
    (_, localDistance, year, consumption) = characterizeDemands demands'
    reifyTechnology = productionReifier priceCube processLibrary intensityCube localLocation localDistance year
    reifyDelivery = deliveryReifier' priceCube processLibrary intensityCube localLocation localDistance year 
    productionsOnsite =
      costedCandidates
        (productionCandidates $ reifyTechnology consumption)
        (productions' Onsite processLibrary)
        demands'
    productionsCentral =
      costedCandidates
        (productionCandidates $ reifyTechnology consumption)
        (productions' Central processLibrary)
        demands'
    deliveries =
      costedCandidates
        (deliveryCandidates $ reifyDelivery consumption)
        (localPathways processLibrary)
        demands'
  in
    LocalContext{..}


cheapestLocally :: LocalContext -> [(Location, Optimum)]
cheapestLocally LocalContext{..} =
  let
    candidatesOnsite = M.elems productionsOnsite
    candidatesCentral = M.elems productionsCentral
    candidatesLocal = M.elems deliveries
    best = minimum [minimum candidatesOnsite, minimum candidatesCentral <> minimum candidatesLocal]
  in
    if null candidatesOnsite && (null candidatesCentral || null candidatesLocal)
      then error ("No eligible technologies in year " ++ show (maximum localYears) ++ ".") -- FIXME: Move to error monad.
      else [(localLocation, snd best)]


costedDeliveryCandidates :: GlobalContext -> LocalContext -> LocalContext -> [(Pathway, CostedOptimum)]
costedDeliveryCandidates GlobalContext{..} localContextLeft localContextRight =
  let
    Network{..} = network
    path = paths M.! (localLocation localContextLeft, localLocation localContextRight)
    year' = fst $ M.findMin $ M.filter (> 0) $ localDemands localContextLeft
    consumption' = maximum $ M.elems $ localDemands localContextLeft
    distance' = sum $ snd <$> linkIds path
    transmissionReifier' =
      pathwayReifier
        (\_ _ -> True)
        processLibrary
        (
          technologyReifier
            processLibrary
            (localize intensityCube $ localLocation localContextLeft) -- FIXME
            (\m y -> maybe 0 (fPrice <:) $ priceCube `evaluate` (fMaterial =: m <+> fYear =: y <+> fLocation =: localLocation localContextLeft)) -- FIXME: extrapolate
        )
        year'
        consumption'
        distance'
        (Infrastructure (location (localLocation localContextLeft) ++ " @ " ++ show year'), path)
  in
    [
      let
        (flows, cashes, impacts) = unzip3 $ map (uncurry operate) $ M.toList $ localDemands localContextLeft
      in
        (
          pathway
        , (
            Sum $ sum $ (fSale <:) <$> concat flows
          , Optimum construction (concat flows) (concat cashes) (concat impacts)
          )
        )
    |
      pathway <- toList $ pathways processLibrary
    , let z = transmissionReifier' pathway
    , isJust z
    , let Just (construction, operate) = z
    ]


cheapestDoubly :: GlobalContext -> LocalContext -> LocalContext -> Maybe (FieldRec '[FConsumption, FOptimum])
cheapestDoubly globalContext localContextLeft localContextRight =
  trace' (show $ costedDeliveryCandidates globalContext localContextLeft localContextRight)
    Nothing
