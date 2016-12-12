-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.VehicleStock
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for vehicle stock modeling.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module SERA.Service.VehicleStock (
-- * Configuration
  ConfigStock(..)
, SurvivalData(..)
, TravelData(..)
-- * I/O
, survivalCube
, travelCube
-- * Computation
, stockMain
, stockInvertMain
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..))
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform, verboseReadFieldCubeSource, verboseWriteFieldCubeSource)
import SERA.Service ()
import SERA.Vehicle.Stock (computeStock, inferSales)
import SERA.Vehicle.Stock.Types (AnnualTravelCube, SurvivalCube)
import VISION.Survival (survivalLDV, survivalHDV)
import VISION.Travel (travelLDV)


-- | Vehicle survival data.
data SurvivalData =
    VISION_LDV_Survival -- ^ LDV survival function from the VISION model.
  | VISION_HDV_Survival -- ^ HDV survival function from the VISION model.
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON SurvivalData

instance ToJSON SurvivalData

instance Default SurvivalData where
  def = VISION_LDV_Survival


-- | Read survival data.
survivalCube :: (IsString e, MonadError e m, MonadIO m)
             => Maybe (DataSource SurvivalData) -- ^ Source of survival data.
             -> m SurvivalCube                  -- ^ Data cube for survival data.
survivalCube  Nothing                                 = return survivalLDV
survivalCube (Just (BuiltinData VISION_LDV_Survival)) = do
                                                          inform "Using built-in VISION LDV survival data."
                                                          return survivalLDV
survivalCube (Just (BuiltinData VISION_HDV_Survival)) = do
                                                          inform "Using built-in VISION MDV/HDV survival data."
                                                          return survivalHDV
survivalCube (Just source                           ) = verboseReadFieldCubeSource "survival" source


-- | Vehicle travel data.
data TravelData =
    VISION_LDV_Travel -- ^ LDV travel function from the VISION model.
  | VISION_HDV_Travel -- ^ HDV travel function from the VISION model.
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON TravelData

instance ToJSON TravelData

instance Default TravelData where
  def = VISION_LDV_Travel


-- | Read travel data.
travelCube :: (IsString e, MonadError e m, MonadIO m)
           => Maybe (DataSource TravelData) -- ^ Source of travel data.
           -> m AnnualTravelCube            -- ^ Data cube for travel data.
travelCube  Nothing                               = return travelLDV
travelCube (Just (BuiltinData VISION_LDV_Travel)) = do
                                                      inform "Using built-in VISION LDV annual travel data."
                                                      return travelLDV
travelCube (Just source                         ) = verboseReadFieldCubeSource "annual travel" source


-- | Configuration for vehicle stock modeling.
data ConfigStock =
  ConfigStock
  {
    regionalSalesSource  :: DataSource Void                  -- ^ Source for egional sales.
  , marketShareSource    :: DataSource Void                  -- ^ Source for arket shares.
  , survivalSource       :: Maybe (DataSource SurvivalData)  -- ^ Source for ehicle survival.
  , annualTravelSource   :: Maybe (DataSource TravelData  )  -- ^ Source for annual travel.
  , fuelSplitSource      :: DataSource Void                  -- ^ Source for fuel splits.
  , fuelEfficiencySource :: DataSource Void                  -- ^ Source for fuel efficiency.
  , emissionRateSource   :: DataSource Void                  -- ^ Source for emission rates.
  , salesSource          :: DataSource Void                  -- ^ Source for vehicle sales.
  , stockSource          :: DataSource Void                  -- ^ Source for vehicle stock.
  , energySource         :: DataSource Void                  -- ^ Source for energy consumed.
  , emissionSource       :: DataSource Void                  -- ^ Source for pollutants emitted.
  , priorYears           :: Maybe Int                        -- ^ Number of prior years to consider when inverting vehicle stock.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigStock

instance ToJSON ConfigStock


-- | Compute vehicle stock.
stockMain :: (IsString e, MonadError e m, MonadIO m)
               => ConfigStock -- ^ Configuration data.
               -> m ()        -- ^ Action to compute vehicle stock.
stockMain ConfigStock{..} =
  do
    regionalSales <- verboseReadFieldCubeSource "regional sales" regionalSalesSource
    marketShare <- verboseReadFieldCubeSource "market share" marketShareSource
    survival <- survivalCube survivalSource
    annualTravel <- travelCube annualTravelSource
    fuelSplit <- verboseReadFieldCubeSource "fuel split" fuelSplitSource
    fuelEfficiency <- verboseReadFieldCubeSource "fuel efficiency" fuelEfficiencySource
    emissionRate <- verboseReadFieldCubeSource "emission rate" emissionRateSource
    inform "Computing vehicle stock . . ."
    let
      (sales, stock, energy, emission) = computeStock regionalSales marketShare survival annualTravel fuelSplit fuelEfficiency emissionRate
    verboseWriteFieldCubeSource "vehicle sales" salesSource sales
    verboseWriteFieldCubeSource "vehicle Stock" stockSource stock
    verboseWriteFieldCubeSource "energy consumption" energySource energy
    verboseWriteFieldCubeSource "emssion of pollutants" emissionSource emission


-- | Invert a vehicle stock computation.
stockInvertMain :: (IsString e, MonadError e m, MonadIO m)
            => ConfigStock -- ^ Configuration data.
            -> m ()        -- ^ Action to invert a vehicle stock computation.
stockInvertMain ConfigStock{..} =
  do
    stock <- verboseReadFieldCubeSource "regional stocks" stockSource
    survival <- survivalCube survivalSource
    inform "Computing vehicle sales . . ."
    let
      (regionalSales, marketShare) = inferSales (fromMaybe 0 priorYears) survival stock
    verboseWriteFieldCubeSource "regional sales" regionalSalesSource regionalSales
    verboseWriteFieldCubeSource "market shares" marketShareSource marketShare
