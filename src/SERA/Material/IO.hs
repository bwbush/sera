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


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Material.IO (
  readPrices
, readIntensities
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.String (IsString)
import SERA.Material.Types (IntensityCube, PriceCube)
import SERA.Network.Types (FZone)
import SERA.Types (FYear)


readPrices :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m (PriceCube '[FYear, FZone])
readPrices = (mconcat <$>) . mapM readFieldCubeFile


readIntensities :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m (IntensityCube '[FZone])
readIntensities = (mconcat <$>) . mapM readFieldCubeFile
