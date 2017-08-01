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
import SERA.Material.Prices
import SERA.Material.Types
import SERA.Network.Types
import SERA.Process.Reification.Pathway
import SERA.Process.Reification.Technology
import SERA.Process.Types
import SERA.Types


type FOptimum = '("Optimum", ([Construction], [Flow], [Cash], [Impact]))


fOptimum :: SField FOptimum
fOptimum = SField


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
    reifyTechnology =
      technologyReifier
        processLibrary
        (localize intensityCube loc)
        (\m y -> maybe 0 (fPrice <:) $ priceCube `evaluate` (fMaterial =: m <+> fYear =: y <+> fLocation =: loc)) -- FIXME: extrapolate
        (fInfrastructure =: Infrastructure (location loc ++ " @ " ++ show year) <+> fLocation =: loc)
        year
        consumption
        (2 * sqrt area)
    candidates =
      [
        (
          sum $ (fSale <:) <$> flows
        , ([construction], flows, concat cashes, concat impacts)
        )
      |
        tech <- toList $ productions' Onsite processLibrary
      , let reification = reifyTechnology tech
      , isJust reification
      , let Just (construction, operate) = reification
      , let (flows, cashes, impacts) = unzip3 $ (\rec -> operate (fYear <: rec) (fConsumption <: rec)) <$> demands
      ]
    reifyPathway =
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
    candidates' =
      [
        (
          sum $ (fSale <:) <$> flows
        , (construction : construction', concat (flows : flows'), concat (cashes ++ cashes'), concat (impacts ++ impacts'))
        )
      |
        tech <- toList $ productions' Central processLibrary
      , let reification = reifyTechnology tech
      , isJust reification
      , let Just (construction, operate) = reification
      , let (flows, cashes, impacts) = unzip3 $ (\rec -> operate (fYear <: rec) (fConsumption <: rec)) <$> demands
      , path <- toList $ localPathways processLibrary
      , let reification' = reifyPathway (Infrastructure (location loc ++ " @ " ++ show year), GenericPath loc [(loc, 2 * sqrt area)] loc) path
      , isJust reification'
      , let Just (construction', operate') = reification'
      , let (flows', cashes', impacts') = unzip3 $ (\rec -> operate' (fYear <: rec) (fConsumption <: rec)) <$> demands
      ]
    best = minimum $ fst <$> candidates ++ candidates'
  in
    if null candidates && null candidates'
      then error ("No eligible technologies in year " ++ show year ++ ".") -- FIXME: Move to error monad.
      else     fYear =: year
           <+> fConsumption =: consumption
           <+> fOptimum =: snd (head $ dropWhile ((/= best) . fst) $ candidates ++ candidates')
