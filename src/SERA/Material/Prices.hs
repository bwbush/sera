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


module SERA.Material.Prices (
-- * Input/output
  readPrices
-- * Manipulation
, rezonePrices
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.String (IsString)
import SERA.Material.Types (PriceCube, FZone, ZoneCube)


readPrices :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m (PriceCube '[FZone])
readPrices = (mconcat <$>) . mapM readFieldCubeFile


rezonePrices :: PriceCube '[FZone] -> ZoneCube key -> PriceCube key
rezonePrices = undefined
