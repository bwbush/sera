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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Material.Prices (
-- * Manipulation
  rezonePrices
) where


import Data.Daft.Vinyl.FieldCube ((⋈), κ, ω)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec, (=:))
import SERA.Material.Types (fPrice, PriceCube)
import SERA.Network.Types (FLocation, FZone, ZoneCube)
import SERA.Types (fFraction)


rezonePrices :: PriceCube '[FZone]
             -> ZoneCube '[FLocation]
             -> PriceCube '[FLocation]
rezonePrices prices zones = -- FIXME: Generalize this to `key` instead of `FLocation`.
  κ (ω zones :: Set (FieldRec '[FZone])) combine
    $ prices ⋈ zones
    where
      combine _ fps = fPrice =: sum [fFraction <: fp * fPrice <: fp | fp <- fps] -- FIXME: Generalize.
