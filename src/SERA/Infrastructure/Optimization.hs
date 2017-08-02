{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}


module SERA.Infrastructure.Optimization
-- FIXME
where


import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Maybe (catMaybes)
import Data.Monoid (Sum(..), (<>))
import Data.Set (Set, toList)
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


costCandidate :: ([Construction], PathwayOperation) -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> (Sum Double, ([Construction], [Flow], [Cash], [Impact]))
costCandidate (construction, operate) demands =
  let
    (flows, cashes, impacts) = unzip3 $ (\rec -> operate (fYear <: rec) (fConsumption <: rec)) <$> demands
  in
    (
      Sum $ sum $ (fSale <:) <$> concat flows
    , (construction, concat flows, concat cashes, concat impacts)
    )


costedCandidates :: ([a] -> [([Construction], PathwayOperation)])
                 -> Set a
                 -> [FieldRec '[FLocation, FYear, FConsumption, FArea]]
                 -> [(Sum Double, ([Construction], [Flow], [Cash], [Impact]))]
costedCandidates makeCandidates xs demands =
  [
    costCandidate candidate demands
  |
    candidate <- makeCandidates $ toList xs
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


cheapestLocally :: PriceCube '[FLocation] -> ProcessLibrary -> IntensityCube '[FLocation] -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> FieldRec '[FYear, FConsumption, FOptimum]
cheapestLocally priceCube processLibrary intensityCube demands =
  let
    (loc, distance, year, consumption) = characterizeDemands demands
    reifyTechnology = productionReifier priceCube processLibrary intensityCube loc distance year
    reifyDelivery = deliveryReifier' priceCube processLibrary intensityCube loc distance year 
    candidatesOnsite =
      costedCandidates
        (productionCandidates $ reifyTechnology consumption)
        (productions' Onsite processLibrary)
        demands
    candidatesCentral =
      costedCandidates
        (productionCandidates $ reifyTechnology consumption)
        (productions' Central processLibrary)
        demands
    candidatesLocal =
      costedCandidates
        (deliveryCandidates $ reifyDelivery consumption)
        (localPathways processLibrary)
        demands
    best = minimum [minimum candidatesOnsite, minimum candidatesCentral <> minimum candidatesLocal]
  in
    if null candidatesOnsite && (null candidatesCentral || null candidatesLocal)
      then error ("No eligible technologies in year " ++ show year ++ ".") -- FIXME: Move to error monad.
      else     fYear =: year
           <+> fConsumption =: consumption
           <+> fOptimum =: snd best


cheapestPair :: PriceCube '[FLocation] -> ProcessLibrary -> IntensityCube '[FLocation] -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> FieldRec '[FYear, FConsumption, FOptimum]
cheapestPair priceCube processLibrary intensityCube demandsLeft demandsRight =
  let
    (locLeft , distanceLeft , yearLeft , consumptionLeft ) = characterizeDemands demandsLeft
    (locRight, distanceRight, yearRight, consumptionRight) = characterizeDemands demandsRight
    year = maximum [yearLeft, yearRight]
    reifyTechnologyLeft  = productionReifier priceCube processLibrary intensityCube locLeft  distanceLeft  year
    reifyTechnologyRight = productionReifier priceCube processLibrary intensityCube locRight distanceRight year
  in
    undefined
