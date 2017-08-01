{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}


module SERA.Infrastructure.Optimization
-- FIXME
where


import Control.Arrow ((***))
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set (toList)
import Data.Vinyl.Derived (FieldRec, SField(..))
import SERA.Infrastructure.Types
import SERA.Material.Prices
import SERA.Material.Types
import SERA.Network.Types
import SERA.Process.Reification.Pathway
import SERA.Process.Reification.Technology (TechnologyOperation, technologyReifier)
import SERA.Process.Types
import SERA.Types


type FOptimum = '("Optimum", ([Construction], [Flow], [Cash], [Impact]))


fOptimum :: SField FOptimum
fOptimum = SField


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


productionCandidates :: TechnologyReifier -> [Technology] -> [([Construction], PathwayOperation)]
productionCandidates reifyTechnology techs =
  catMaybes
    [
      do
        (construction, operate) <- reifyTechnology tech
        return ([construction], \y c -> let (f, cs, is) = operate y c in ([f], cs, is))
    |
      tech <- techs
    ]


deliveryCandidates :: DeliveryReifier -> [Pathway] -> [([Construction], PathwayOperation)]
deliveryCandidates reifyDelivery paths' =
  catMaybes
    [
      do
        (construction, operate) <- reifyDelivery path
        return (construction, operate)
    |
      path <- paths'
    ]


costCandidate :: ([Construction], PathwayOperation) -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> (Double, ([Construction], [Flow], [Cash], [Impact]))
costCandidate (construction, operate) demands =
  let
    (flows, cashes, impacts) = unzip3 $ (\rec -> operate (fYear <: rec) (fConsumption <: rec)) <$> demands
  in
    (
      sum $ (fSale <:) <$> concat flows
    , (construction, concat flows, concat cashes, concat impacts)
    )


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


instance Monoid (Double, ([Construction], [Flow], [Cash], [Impact])) where
  mempty = (0, mempty)
  mappend (x, xs) (y, ys) = (x + y, xs `mappend` ys)


cheapestLocally :: PriceCube '[FLocation] -> ProcessLibrary -> IntensityCube '[FLocation] -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> FieldRec '[FYear, FConsumption, FOptimum]
cheapestLocally priceCube processLibrary intensityCube demands =
  let
    (loc, distance, year, consumption) = characterizeDemands demands
    reifyTechnology = productionReifier priceCube processLibrary intensityCube loc distance year
    candidates =
      [
        costCandidate candidate demands
      |
        candidate <- productionCandidates (reifyTechnology consumption) $ toList $ productions' Onsite processLibrary
      ]
    reifyDelivery = deliveryReifier' priceCube processLibrary intensityCube loc distance year 
    candidates' =
      [
        (
          cost + cost'
        , (construction, flows, cashes, impacts) <> (construction', flows', cashes', impacts')
        )
      |
        candidate <- productionCandidates (reifyTechnology consumption) $ toList $ productions' Central processLibrary
      , let (cost, (construction, flows, cashes, impacts)) = costCandidate candidate demands
      , candidate' <- deliveryCandidates (reifyDelivery consumption) $ toList $ localPathways processLibrary
      , let (cost', (construction', flows', cashes', impacts')) = costCandidate candidate' demands
      ]
    best = minimum $ fst <$> candidates ++ candidates'
  in
    if null candidates && null candidates'
      then error ("No eligible technologies in year " ++ show year ++ ".") -- FIXME: Move to error monad.
      else     fYear =: year
           <+> fConsumption =: consumption
           <+> fOptimum =: snd (head $ dropWhile ((/= best) . fst) $ candidates ++ candidates')


cheapestPair :: PriceCube '[FLocation] -> ProcessLibrary -> IntensityCube '[FLocation] -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> FieldRec '[FYear, FConsumption, FOptimum]
cheapestPair priceCube processLibrary intensityCube demandsLeft demandsRight =
  let
    (locLeft, distanceLeft, yearLeft, consumptionLeft) = characterizeDemands demandsLeft
    (locRight, distanceRight, yearRight, consumptionRight) = characterizeDemands demandsRight
    year = maximum [yearLeft, yearRight]
    reifyTechnologyLeft = productionReifier priceCube processLibrary intensityCube locLeft distanceLeft year
    reifyTechnologyRight = productionReifier priceCube processLibrary intensityCube locRight distanceRight year
  in
    undefined
