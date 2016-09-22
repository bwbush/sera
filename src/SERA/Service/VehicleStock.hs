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
-- * Computation
, calculateStock
, invertStock
) where


import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..), withSource)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeSource, writeFieldCubeSource)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
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


survivalCube :: (IsString e, MonadError e m, MonadIO m) => Maybe (DataSource SurvivalData) -> m SurvivalCube
survivalCube  Nothing                                 = return survivalLDV
survivalCube (Just (BuiltinData VISION_LDV_Survival)) = do
                                                          inform "Using built-in VISION LDV survival data."
                                                          return survivalLDV
survivalCube (Just (BuiltinData VISION_HDV_Survival)) = do
                                                          inform "Using built-in VISION MDV/HDV survival data."
                                                          return survivalHDV
survivalCube (Just source                           ) = do
                                                          inform $ "Reading survival from " ++ show source ++ " . . . "
                                                          readFieldCubeSource source


-- | Vehicle travel data.
data TravelData =
    VISION_LDV_Travel -- ^ LDV travel function from the VISION model.
  | VISION_HDV_Travel -- ^ HDV travel function from the VISION model.
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON TravelData

instance ToJSON TravelData

instance Default TravelData where
  def = VISION_LDV_Travel


travelCube :: (IsString e, MonadError e m, MonadIO m) => Maybe (DataSource TravelData) -> m AnnualTravelCube
travelCube  Nothing                               = return travelLDV
travelCube (Just (BuiltinData VISION_LDV_Travel)) = do
                                                      inform "Using built-in VISION LDV annual travel data."
                                                      return travelLDV
travelCube (Just source                         ) = do
                                                        inform $ "Reading annual travel from " ++ show source ++ " . . . "
                                                        readFieldCubeSource source


-- | Configuration for vehicle stock modeling.
data ConfigStock =
  ConfigStock
  {
    regionalSalesSource  :: DataSource Void                  -- ^ Regional sales.
  , marketShareSource    :: DataSource Void                  -- ^ Market shares.
  , survivalSource       :: Maybe (DataSource SurvivalData)  -- ^ Vehicle survival.
  , annualTravelSource   :: Maybe (DataSource TravelData  )  -- ^ Annual travel.
  , fuelSplitSource      :: DataSource Void                  -- ^ Fuel splits.
  , fuelEfficiencySource :: DataSource Void                  -- ^ Fuel efficiency.
  , emissionRateSource   :: DataSource Void                  -- ^ Emission rates.
  , salesSource          :: DataSource Void                  -- ^ Vehicle sales.
  , stockSource          :: DataSource Void                  -- ^ Vehicle stock.
  , energySource         :: DataSource Void                  -- ^ Energy consumed.
  , emissionSource       :: DataSource Void                  -- ^ Pollutants emitted.
  , priorYears           :: Maybe Int                        -- ^ Number of prior years to consider when inverting vehicle stock.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigStock

instance ToJSON ConfigStock


-- | Compute vehicle stock.
calculateStock :: (IsString e, MonadError e m, MonadIO m)
               => ConfigStock -- ^ Configuration data.
               -> m ()        -- ^ Action to compute vehicle stock.
calculateStock ConfigStock{..} =
  do
    inform $ "Reading regional sales from " ++ show regionalSalesSource ++ " . . ."
    regionalSales <- readFieldCubeSource regionalSalesSource
    inform $ "Reading market share from " ++ show marketShareSource ++ " . . . "
    survival <- survivalCube survivalSource
    marketShare <- readFieldCubeSource marketShareSource
    annualTravel <- travelCube annualTravelSource
    inform $ "Reading fuel split from " ++ show fuelSplitSource ++ " . . . "
    fuelSplit <- readFieldCubeSource fuelSplitSource
    inform $ "Reading fuel efficiency from " ++ show fuelEfficiencySource ++ " . . . "
    fuelEfficiency <- readFieldCubeSource fuelEfficiencySource
    inform $ "Reading emission rate from " ++ show emissionRateSource ++ " . . . "
    emissionRate <- readFieldCubeSource emissionRateSource
    let
      (sales, stock, energy, emission) = computeStock regionalSales marketShare survival annualTravel fuelSplit fuelEfficiency emissionRate
    withSource salesSource $ \source -> do
      inform $ "Writing vehicle sales to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source sales
    withSource stockSource $ \source -> do
      inform $ "Writing vehicle stocks to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source stock
    withSource energySource $ \source -> do
      inform $ "Writing energy consumption to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source energy
    withSource emissionSource $ \source -> do
      inform $ "Writing emission of pollutants to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source emission


-- | Invert a vehicle stock computation.
invertStock :: (IsString e, MonadError e m, MonadIO m)
            => ConfigStock -- ^ Configuration data.
            -> m ()        -- ^ Action to invert a vehicle stock computation.
invertStock ConfigStock{..} =
  do
    inform $ "Reading regional stocks from " ++ show stockSource ++ " . . ."
    stock <- readFieldCubeSource stockSource
    survival <- survivalCube survivalSource
    inform "Computing vehicle sales . . ."
    let
      (regionalSales, marketShare) = inferSales (fromMaybe 0 priorYears) survival stock
    withSource regionalSalesSource $ \source -> do
      inform $ "Writing regional sales to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source regionalSales
    withSource marketShareSource $ \source -> do
      inform $ "Writing market shares to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source marketShare
