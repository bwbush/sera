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
import SERA (inform, verboseReadFieldCubeSource, verboseWriteFieldCubeSource)
import SERA.Scenario.Regionalization (RegionalizationParameters, regionalize)
import SERA.Service ()
import SERA.Service.VehicleStock (SurvivalData, TravelData, survivalCube, travelCube)
import SERA.Vehicle.Stock (recomputeStock)


-- | Configuration for regionalizing demand.
data ConfigRegionalization =
  ConfigRegionalization
  {
    regionalizationParameters   :: RegionalizationParameters -- ^ Parameters for regionalizing demand.
  , regionalIntroductionsSource :: DataSource Void           -- ^ Source of regional introductions.
  , totalStockSource            :: DataSource Void           -- ^ Source of total stock.
  , survivalSource       :: Maybe (DataSource SurvivalData)  -- ^ Source for ehicle survival.
  , annualTravelSource   :: Maybe (DataSource TravelData  )  -- ^ Source for annual travel.
  , fuelSplitSource      :: DataSource Void                  -- ^ Source for fuel splits.
  , fuelEfficiencySource :: DataSource Void                  -- ^ Source for fuel efficiency.
  , emissionRateSource   :: DataSource Void                  -- ^ Source for emission rates.
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
    inform "Regionalizing vehicle sales . . ."
    let
      (regionalSales, travelReduction) = regionalize regionalizationParameters regionalIntroductions totalStock
    survival <- survivalCube survivalSource
    annualTravel <- travelCube annualTravelSource
    fuelSplit <- verboseReadFieldCubeSource "fuel split" fuelSplitSource
    fuelEfficiency <- verboseReadFieldCubeSource "fuel efficiency" fuelEfficiencySource
    emissionRate <- verboseReadFieldCubeSource "emission rate" emissionRateSource
    inform "Computing vehicle stock . . ."
    let
      (_, regionalStock, _, _) = recomputeStock regionalSales travelReduction survival annualTravel fuelSplit fuelEfficiency emissionRate
    verboseWriteFieldCubeSource "regional stock" regionalStockSource regionalStock
