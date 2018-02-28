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
import Control.Monad.Log (logError, logInfo, logNotice, logWarning)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.String (IsString)
import SERA (SeraLog, checkPresent, readConcat)
import SERA.Material.Types (IntensityCube, PriceCube, fMaterial, fUpstreamMaterial)
import SERA.Network.Types (Network(..), FZone, fZone)
import SERA.Process.Types (ProcessLibrary(..))
import SERA.Util (extractKey)

import qualified Data.Set as S (union, unions)


readPrices :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (PriceCube '[FZone])
readPrices = readConcat "prices" "price key"


checkPrices :: SeraLog m => Network -> ProcessLibrary -> IntensityCube '[FZone] -> PriceCube '[FZone] -> m ()
checkPrices Network{..} ProcessLibrary{..} intensityCube priceCube =
  do
    logInfo "Checking prices . . ."
    let
      priceMaterials      = extractKey (fMaterial <:)         priceCube
      inputMaterials      = extractKey (fMaterial <:)         processInputCube
      outputMaterials     = extractKey (fMaterial <:)         processOutputCube
      intensityMaterials  = extractKey (fMaterial <:)         intensityCube
      intensityMaterials' = extractKey (fUpstreamMaterial <:) intensityCube
    checkPresent
      logError
      "Prices"
      (extractKey (fZone <:) priceCube)
      "network zones"
      (extractKey (fZone <:) zoneCube)
    checkPresent
      logNotice
      "Process inputs"
      inputMaterials
      "prices"
      priceMaterials
    checkPresent
      logNotice
      "Process outputs"
      outputMaterials
      "prices"
      priceMaterials
    checkPresent
      logNotice
      "Upstream emission intensities"
      (intensityMaterials `S.union` intensityMaterials')
      "prices"
      priceMaterials
    checkPresent
      logNotice
      "Prices"
      priceMaterials
      "process inputs, process outputs, or upstream emissions intensities"
      $ S.unions [inputMaterials, outputMaterials, intensityMaterials, intensityMaterials']


readIntensities :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (IntensityCube '[FZone])
readIntensities = readConcat "upstream emission intensities" "intensity key"


checkIntensities :: SeraLog m => Network -> ProcessLibrary -> IntensityCube '[FZone] -> m ()
checkIntensities Network{..} ProcessLibrary{..} intensityCube =
  do
    logInfo "Checking upstream emission intensities . . ."
    let
      inputMaterials      = extractKey (fMaterial <:)         processInputCube
      outputMaterials     = extractKey (fMaterial <:)         processOutputCube
      intensityMaterials  = extractKey (fMaterial <:)         intensityCube
    checkPresent
      logError
      "Upstream emission intensities"
      (extractKey (fZone <:) intensityCube)
      "network zones"
      (extractKey (fZone <:) zoneCube)
    checkPresent
      logWarning
      "Upstream emissions intensities"
      intensityMaterials
      "process inputs or process outputs"
      (inputMaterials `S.union` outputMaterials)
