-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.HydrogenSizing
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for computing hydrogen statiomn sizes for a scenario.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Service.HydrogenProduction (
-- * Configuration
  ConfigProduction(..)
-- * Computation
, productionMain
) where


import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.Source (DataSource(..))
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (verboseReadFieldCubeSource, verboseWriteFieldCubeSource)
import SERA.Material.IO (readIntensities, readPrices)
import SERA.Material.Prices (materials)
import SERA.Process (ProcessLibraryFiles, deliveries, pathways, productions, readProcessLibrary)
import SERA.Refueling.Hydrogen.Sizing (StationCapacityParameters)
import SERA.Scenario.Grants (allocateGrants)
import SERA.Scenario.HydrogenSizing (CapitalCostParameters, SitePreparationParameters, sizeStations)
import SERA.Service ()


-- | Configuration for hydrogen station sizing.
data ConfigProduction =
  ConfigProduction
  {
    priceFiles          :: [FilePath]
  , intensityFiles      :: [FilePath]
  , processLibraryFiles :: [ProcessLibraryFiles]
  , pathwayFiles        :: [FilePath]
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigProduction

instance ToJSON ConfigProduction


-- | Compute hydrogen station sizes.
productionMain :: (IsString e, MonadError e m, MonadIO m)
                       => ConfigProduction -- ^ Configuration data.
                       -> m ()                 -- ^ Action to compute the station sizes.
productionMain ConfigProduction{..} =
  do
    priceCube <- readPrices priceFiles
    intensityCube <- readIntensities intensityFiles
    processLibrary <- readProcessLibrary processLibraryFiles pathwayFiles
    liftIO
      $ do
        let
          list label content =
            do
              putStrLn ""
              putStrLn $ label ++ ":"
              mapM_ (putStrLn . ("  " ++) . show) content
        list "Material"   $ materials   priceCube
        list "Production" $ productions processLibrary
        list "Delivery"   $ deliveries  processLibrary
        list "Pathway"    $ pathways    processLibrary
