{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Material (
-- * Access
  materials
, upstreamMaterials
, makePricer
) where


import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Vinyl.Derived (FieldRec)
import SERA.Material.Types (fMaterial, fPrice, PriceCube, Pricer)
import SERA.Types (fYear)
import Data.Daft.Vinyl.FieldCube ((⋈), κ, ω)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set, toList)
import Data.Vinyl.Derived (FieldRec)
import SERA.Material.Types (Material, FMaterial, fMaterial, fPrice, PriceCube, FUpstreamMaterial, fUpstreamMaterial)
import SERA.Network.Types (FZone, ZoneCube)
import SERA.Types (fFraction, FRegion, FYear)


--materials :: PriceCube a -> [Material]
materials priceCube =
  fmap (fMaterial <:)
    . toList
    $ (ω priceCube :: Set (FieldRec '[FMaterial]))


--upstreamMaterials :: PriceCube a -> [Material]
upstreamMaterials priceCube =
  fmap (fUpstreamMaterial <:)
    . toList
    $ (ω priceCube :: Set (FieldRec '[FUpstreamMaterial]))


makePricer :: Ord (FieldRec key) => PriceCube key -> FieldRec key -> Pricer
makePricer priceCube key material year =
  maybe 0 (fPrice <:) 
    . evaluate priceCube
    $ fMaterial =: material <+> fYear =: year <+> key
