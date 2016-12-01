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


module SERA.Service.HydrogenSizing (
-- * Configuration
  ConfigHydrogenSizing(..)
-- * Computation
, hydrogenSizingMain
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.Source (DataSource(..))
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (verboseReadFieldCubeSource, verboseWriteFieldCubeSource)
import SERA.Refueling.Hydrogen.Sizing (StationCapacityParameters)
import SERA.Scenario.HydrogenSizing (CapitalCostParameters, SitePreparationParameters, sizeStations)
import SERA.Service ()


-- | Configuration for hydrogen station sizing.
data ConfigHydrogenSizing =
  ConfigHydrogenSizing
  {
    externalCapacitySource      :: DataSource Void           -- ^ Source for external station capacity.
  , regionalIntroductionsSource :: DataSource Void           -- ^ Source for regional introduction years.
  , regionalStockSource         :: DataSource Void           -- ^ Source for regional vehicle stock.
  , overrideStationsSource      :: DataSource Void           -- ^ Source for manually specified stations.
  , stationsSummarySource       :: DataSource Void           -- ^ Source for station summary.
  , stationsDetailsSource       :: DataSource Void           -- ^ Source for station details.
  , sizingParameters            :: StationCapacityParameters -- ^ Station capacity parameters.
  , capitalCostParameters       :: CapitalCostParameters     -- ^ Station capital cost parameters.
  , sitePreparationParameters   :: SitePreparationParameters -- ^ Site preparation parameters.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigHydrogenSizing

instance ToJSON ConfigHydrogenSizing


-- | Compute hydrogen station sizes.
hydrogenSizingMain :: (IsString e, MonadError e m, MonadIO m)
                       => ConfigHydrogenSizing -- ^ Configuration data.
                       -> m ()                 -- ^ Action to compute the station sizes.
hydrogenSizingMain ConfigHydrogenSizing{..} =
  do
    externalCapacity <- verboseReadFieldCubeSource "external capacity" externalCapacitySource
    regionalIntroductions <- verboseReadFieldCubeSource "regional introductions" regionalIntroductionsSource
    stock <- verboseReadFieldCubeSource "vehicle stock" regionalStockSource
    overrides <- verboseReadFieldCubeSource "overridden stations" overrideStationsSource
    let
      (details, summary) = sizeStations sizingParameters capitalCostParameters sitePreparationParameters externalCapacity overrides regionalIntroductions stock
    verboseWriteFieldCubeSource "station summary" stationsSummarySource summary
    verboseWriteFieldCubeSource "station details" stationsDetailsSource details
