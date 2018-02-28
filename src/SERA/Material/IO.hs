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
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Material.IO (
  readPrices
, readIntensities
, checkPrices
, checkIntensities
) where


import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logError)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.String (IsString)
import SERA (SeraLog, checkPresent, readConcat)
import SERA.Material.Types (IntensityCube, PriceCube)
import SERA.Network.Types (Network(..), FZone, fZone)
import SERA.Util (extractKey)


readPrices :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (PriceCube '[FZone])
readPrices = readConcat "prices" "price key"


checkPrices :: SeraLog m => Network -> PriceCube '[FZone] -> m ()
checkPrices Network{..} priceCube =
  checkPresent
    logError
    "Prices"
    (extractKey (fZone <:) priceCube)
    "network zones"
    (extractKey (fZone <:) zoneCube)


readIntensities :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (IntensityCube '[FZone])
readIntensities = readConcat "upstream emission intensities" "intensity key"


checkIntensities :: SeraLog m => Network -> IntensityCube '[FZone] -> m ()
checkIntensities Network{..} intensityCube =
  checkPresent
    logError
    "Upstream emission intensities"
    (extractKey (fZone <:) intensityCube)
    "network zones"
    (extractKey (fZone <:) zoneCube)
