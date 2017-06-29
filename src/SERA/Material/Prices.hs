-----------------------------------------------------------------------------
--
-- Module      :  SERA.Energy.Prices
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Energy prices.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Material.Prices (
-- * Access
  materials
-- * Manipulation
, rezonePrices
) where


import Data.Daft.Vinyl.FieldCube ((⋈), κ, ω)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set, toList)
import Data.Vinyl.Derived (FieldRec, (=:))
import SERA.Material.Types (Material, FMaterial, fMaterial, fPrice, PriceCube)
import SERA.Network.Types (FZone, ZoneCube)
import SERA.Types (fFraction, FRegion, FYear)


materials :: PriceCube a -> [Material]
materials priceCube =
  fmap (fMaterial <:)
    . toList
    $ (ω priceCube :: Set (FieldRec '[FMaterial]))


rezonePrices :: PriceCube '[FYear, FZone]
             -> ZoneCube '[FRegion]
             -> PriceCube '[FYear, FRegion]
rezonePrices prices zones = -- FIXME: Generalize this to `key` instead of `FRegion`.
  κ (ω zones :: Set (FieldRec '[FZone])) combine
    $ prices ⋈ zones
    where
      combine _ fps = fPrice =: sum [fFraction <: fp * fPrice <: fp | fp <- fps] -- FIXME: Generalize.
