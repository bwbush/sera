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
-- * Input/output
  readPrices
-- * Manipulation
, rezonePrices
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Daft.Vinyl.FieldCube ((⋈), κ, ω)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.Set (Set)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import SERA.Material.Types (PriceCube, FZone, ZoneCube)
import SERA.Types (FRegion, FYear)


readPrices :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m (PriceCube '[FYear, FZone])
readPrices = (mconcat <$>) . mapM readFieldCubeFile


rezonePrices :: PriceCube '[FYear, FZone]
             -> ZoneCube '[FRegion]
             -> PriceCube '[FYear, FRegion]
rezonePrices prices zones = -- FIXME: Generalize this to `key` instead of `FRegion`.
  κ (ω zones :: Set (FieldRec '[FZone])) (const head)
    $ prices ⋈ zones