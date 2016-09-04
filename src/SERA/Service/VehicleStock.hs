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
import Data.Daft.Vinyl.FieldCube ((⋈), fromRecords, toRecords)
import Data.Default (Default(..))
import Data.List (nub)
import Data.String (IsString)
import Data.Vinyl.Lens (rcast)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Service ()
import SERA.Vehicle.Stock (computeStock, universe)
import VISION.Survival (survivalMHD)


data SurvivalData = VISION_LDV | VISION_HDV
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON SurvivalData

instance ToJSON SurvivalData

instance Default SurvivalData where
  def = VISION_LDV


data ConfigStock =
  ConfigStock
  {
    regionalSalesSource  :: DataSource Void
  , marketShareSource    :: DataSource Void
  , survivalSource       :: Maybe (DataSource SurvivalData)
  , annualTravelSource   :: DataSource Void
  , fuelSplitSource      :: DataSource Void
  , fuelEfficiencySource :: DataSource Void
  , emissionRateSource   :: DataSource Void
  , salesSource          :: DataSource Void
  , stockSource          :: DataSource Void
  , energySource         :: DataSource Void
  , emissionSource       :: DataSource Void
  , priorYears           :: Maybe Int
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigStock

instance ToJSON ConfigStock


calculateStock :: (IsString e, MonadError e m, MonadIO m) => ConfigStock -> m ()
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
      support = universe $ marketShare ⋈ emissionRate
    withSource salesSource $ \source -> do
      inform $ "Writing vehicle sales to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source $ toRecords (nub $ rcast <$> support) sales
    withSource stockSource $ \source -> do
      inform $ "Writing vehicle stocks to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source $ toRecords (nub $ rcast <$> support) stock
    withSource energySource $ \source -> do
      inform $ "Writing energy consumption to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source $ toRecords (nub $ rcast <$> support) energy
    withSource emissionSource $ \source -> do
      inform $ "Writing emission of pollutants to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source $ toRecords (nub $ rcast <$> support) emission


invertStock :: (IsString e, MonadError e m, MonadIO m) => ConfigStock -> m ()
invertStock ConfigStock{..} =
    undefined {-
    inform $ "Reading vehicle stocks from " ++ show stockSource ++ " . . ."
    stock <- readFieldRecSource stockSource
    inform "Computing vehicle sales . . ."
    let
      sales = inferSales (fromMaybe 0 priorYears) undefined stock -- survivalFunction stock
      (regionalSales, shares) = inferMarketShares sales
    withSource salesStockSource $ \source -> do
      inform $ "Writing vehicle sales and stocks to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source sales
    withSource regionalSalesSource $ \source -> do
      inform $ "Writing regional sales to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source regionalSales
    withSource marketSharesSource $ \source -> do
      inform $ "Writing market shares to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source shares
-}
