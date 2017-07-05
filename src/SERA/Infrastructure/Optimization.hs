{-# LANGUAGE DataKinds #-}


module SERA.Infrastructure.Optimization
-- FIXME
where


import Control.Arrow ((***))
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Maybe (isJust)
import Data.Set (toList)
import Data.Vinyl.Derived (FieldRec, SField(..))
import SERA.Infrastructure.Types
import SERA.Material.Types
import SERA.Network.Types
import SERA.Process.Reification.Technology
import SERA.Process.Types
import SERA.Types


type FOptimum = '("Optimum", (Construction, [Flow], [Cash], [Impact]))


fOptimum :: SField FOptimum
fOptimum = SField


cheapestLocally :: PriceCube '[FLocation] -> ProcessLibrary -> [FieldRec '[FLocation, FYear, FConsumption, FArea]] -> FieldRec '[FYear, FConsumption, FOptimum]
cheapestLocally priceCube processLibrary demands =
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
    reifyTechnology =
      technologyReifier
        processLibrary
        (\m y -> maybe 0 (fPrice <:) $ priceCube `evaluate` (fMaterial =: m <+> fYear =: y <+> fLocation =: loc))
        (fInfrastructure =: Infrastructure (location loc ++ " @ " ++ show year) <+> fLocation =: loc)
        year
        consumption
        (2 * sqrt area)
    candidates =
        [
          (
            sum $ (fSale <:) <$> flows
          , (construction, flows, concat cashes, concat impacts)
          )
        |
          tech <- toList $ productions' Onsite processLibrary
        , let reification = reifyTechnology tech
        , isJust reification
        , let Just (construction, operate) = reification
        , let (flows, cashes, impacts) = unzip3 $ (\rec -> operate (fYear <: rec) (fConsumption <: rec)) <$> demands
        ]
    best = minimum $ fst <$> candidates
  in
        fYear =: year
    <+> fConsumption =: consumption
    <+> fOptimum =: snd (head $ dropWhile ((/= best) . fst) candidates)
