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
import Data.String (IsString)
import SERA (SeraLog, readConcat)
import SERA.Material.Types (IntensityCube, PriceCube)
import SERA.Network.Types (FZone)


readPrices :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (PriceCube '[FZone])
readPrices = readConcat "prices" "price key"


readIntensities :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (IntensityCube '[FZone])
readIntensities = readConcat "upstream emission intensities" "intensity key"
