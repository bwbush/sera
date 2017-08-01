{-# LANGUAGE DataKinds #-}


module SERA.Infrastructure.Optimization
-- FIXME
where


import Control.Arrow ((***))
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Maybe (catMaybes)
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
productionReifier priceCube processLibrary intensityCube loc area year consumption =
  technologyReifier
    processLibrary
    (localize intensityCube loc)
    (\m y -> maybe 0 (fPrice <:) $ priceCube `evaluate` (fMaterial =: m <+> fYear =: y <+> fLocation =: loc)) -- FIXME: extrapolate
    (fInfrastructure =: Infrastructure (location loc ++ " @ " ++ show year) <+> fLocation =: loc)
    year
    consumption
    (2 * sqrt area)


deliveryReifier' :: PriceCube '[FLocation] -> ProcessLibrary -> IntensityCube '[FLocation] -> Location -> Double -> Year -> Double -> DeliveryReifier
deliveryReifier' priceCube processLibrary intensityCube loc area year consumption =
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
    (2 * sqrt area)
    (Infrastructure (location loc ++ " @ " ++ show year), GenericPath loc [(loc, 2 * sqrt area)] loc)


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


cheapestLocally :: PriceCube '[FLocation] -> ProcessLibrary -> IntensityCube '[FLocation] -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> FieldRec '[FYear, FConsumption, FOptimum]
cheapestLocally priceCube processLibrary intensityCube demands =
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
    reifyTechnology = productionReifier priceCube processLibrary intensityCube loc area year consumption
    candidates =
      [
        costCandidate candidate demands
      |
        candidate <- productionCandidates reifyTechnology $ toList $ productions' Onsite processLibrary
      ]
    reifyDelivery = deliveryReifier' priceCube processLibrary intensityCube loc area year consumption
    candidates' =
      [
        (
          cost + cost'
        , (construction ++ construction', flows ++ flows', cashes ++ cashes', impacts ++ impacts')
        )
      |
        candidate <- productionCandidates reifyTechnology $ toList $ productions' Central processLibrary
      , let (cost, (construction, flows, cashes, impacts)) = costCandidate candidate demands
      , candidate' <- deliveryCandidates reifyDelivery $ toList $ localPathways processLibrary
      , let (cost', (construction', flows', cashes', impacts')) = costCandidate candidate' demands
      ]
    best = minimum $ fst <$> candidates ++ candidates'
  in
    if null candidates && null candidates'
      then error ("No eligible technologies in year " ++ show year ++ ".") -- FIXME: Move to error monad.
      else     fYear =: year
           <+> fConsumption =: consumption
           <+> fOptimum =: snd (head $ dropWhile ((/= best) . fst) $ candidates ++ candidates')
