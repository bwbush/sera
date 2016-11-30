-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.Regionalization
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for regionalizing demand.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}


module SERA.Service.Regionalization (
-- * Configuration
  ConfigRegionalization(..)
-- * Computation
, regionalizationMain
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..))
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (verboseReadFieldCubeSource, verboseWriteFieldCubeSource)
import SERA.Service ()
import SERA.Scenario.Regionalization (RegionalizationParameters, regionalize)


-- | Configuration for regionalizing demand.
data ConfigRegionalization =
  ConfigRegionalization
  {
    regionalizationParameters   :: RegionalizationParameters -- ^ Parameters for regionalizing demand.
  , regionalIntroductionsSource :: DataSource Void           -- ^ Source of regional introductions.
  , totalStockSource            :: DataSource Void           -- ^ Source of total stock.
  , regionalStockSource         :: DataSource Void           -- ^ Source for regionalized stock.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigRegionalization

instance ToJSON ConfigRegionalization


-- | Compute introduction years.
regionalizationMain :: (IsString e, MonadError e m, MonadIO m)
                         => ConfigRegionalization -- ^ Configuration data.
                         -> m ()                  -- ^ Action to compute the introduction years.
regionalizationMain ConfigRegionalization{..} =
  do
    regionalIntroductions <- verboseReadFieldCubeSource "regional introduction years" regionalIntroductionsSource
    totalStock            <- verboseReadFieldCubeSource "total stock"                 totalStockSource
    let
      regionalStock = regionalize regionalizationParameters regionalIntroductions totalStock
    verboseWriteFieldCubeSource "regionalStock" regionalStockSource regionalStock
