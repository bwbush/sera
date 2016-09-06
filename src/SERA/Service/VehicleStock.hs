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
import Data.Daft.Vinyl.FieldRec (readFieldRecSource, writeFieldRecSource)
import Data.Daft.Vinyl.FieldCube (fromRecords, toKnownRecords)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Service ()
import SERA.Vehicle.Stock (computeStock, inferSales)
import VISION.Survival (survivalMHD)


-- | Vehicle survival data.
data SurvivalData =
    VISION_LDV -- ^ LDV survival function from the VISION model.
  | VISION_HDV -- ^ HDV survival function from the VISION model.
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON SurvivalData

instance ToJSON SurvivalData

instance Default SurvivalData where
  def = VISION_LDV


-- | Configuration for vehicle stock modeling.
data ConfigStock =
  ConfigStock
  {
    regionalSalesSource  :: DataSource Void                  -- ^ Regional sales.
  , marketShareSource    :: DataSource Void                  -- ^ Market shares.
  , survivalSource       :: Maybe (DataSource SurvivalData)  -- ^ Vehicle survival.
  , annualTravelSource   :: DataSource Void                  -- ^ Annual travel.
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
    regionalSales <- fromRecords <$> readFieldRecSource regionalSalesSource
    inform $ "Reading market share from " ++ show marketShareSource ++ " . . . "
    marketShare <- fromRecords <$> readFieldRecSource marketShareSource
    inform $ "Reading annual travel from " ++ show annualTravelSource ++ " . . . "
    annualTravel <- fromRecords <$> readFieldRecSource annualTravelSource
    inform $ "Reading fuel split from " ++ show fuelSplitSource ++ " . . . "
    fuelSplit <- fromRecords <$> readFieldRecSource fuelSplitSource
    inform $ "Reading fuel efficiency from " ++ show fuelEfficiencySource ++ " . . . "
    fuelEfficiency <- fromRecords <$> readFieldRecSource fuelEfficiencySource
    inform $ "Reading emission rate from " ++ show emissionRateSource ++ " . . . "
    emissionRate <- fromRecords <$> readFieldRecSource emissionRateSource
    let
      (sales, stock, energy, emission) = computeStock regionalSales marketShare survivalMHD annualTravel fuelSplit fuelEfficiency emissionRate
    withSource salesSource $ \source -> do
      inform $ "Writing vehicle sales to " ++ show source ++ " . . ."
      void . writeFieldRecSource source $ toKnownRecords sales
    withSource stockSource $ \source -> do
      inform $ "Writing vehicle stocks to " ++ show source ++ " . . ."
      void . writeFieldRecSource source $ toKnownRecords stock
    withSource energySource $ \source -> do
      inform $ "Writing energy consumption to " ++ show source ++ " . . ."
      void . writeFieldRecSource source $ toKnownRecords energy
    withSource emissionSource $ \source -> do
      inform $ "Writing emission of pollutants to " ++ show source ++ " . . ."
      void . writeFieldRecSource source $ toKnownRecords emission


-- | Invert a vehicle stock computation.
invertStock :: (IsString e, MonadError e m, MonadIO m)
            => ConfigStock -- ^ Configuration data.
            -> m ()        -- ^ Action to invert a vehicle stock computation.
invertStock ConfigStock{..} =
  do
    inform $ "Reading regional stocks from " ++ show stockSource ++ " . . ."
    stock <- fromRecords <$> readFieldRecSource stockSource
    inform "Computing vehicle sales . . ."
    let
      (regionalSales, marketShare) = inferSales (fromMaybe 0 priorYears) survivalMHD stock
    withSource regionalSalesSource $ \source -> do
      inform $ "Writing regional sales to " ++ show source ++ " . . ."
      void . writeFieldRecSource source $ toKnownRecords regionalSales
    withSource marketShareSource $ \source -> do
      inform $ "Writing market shares to " ++ show source ++ " . . ."
      void . writeFieldRecSource source $ toKnownRecords marketShare
