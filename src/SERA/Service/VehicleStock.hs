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
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Service ()
import SERA.Vehicle.Stock (computeStock, inferMarketShares, inferSales)
import VISION.Survival (survivalFunction)


data SurvivalData = VISION_LDV | VISION_HDV
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON SurvivalData

instance ToJSON SurvivalData

instance Default SurvivalData where
  def = VISION_LDV


data ConfigStock =
  ConfigStock
  {
    stockSource         :: DataSource Void
  , salesStockSource    :: DataSource Void
  , regionalSalesSource :: DataSource Void
  , marketSharesSource  :: DataSource Void
  , survivalSource      :: Maybe (DataSource SurvivalData)
  , priorYears          :: Maybe Int
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigStock

instance ToJSON ConfigStock


calculateStock :: (IsString e, MonadError e m, MonadIO m) => ConfigStock -> m ()
calculateStock ConfigStock{..} =
  do
    inform $ "Reading regional sales from " ++ show regionalSalesSource ++ " . . ."
    regionalSales <- readFieldRecSource regionalSalesSource
    inform $ "Reading market shares from " ++ show marketSharesSource ++ " . . . "
    marketShares <- readFieldRecSource marketSharesSource
    let
      sales = computeStock survivalFunction regionalSales marketShares
    withSource salesStockSource $ \source -> do
      inform $ "Writing vehicle sales and stocks to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source sales


invertStock :: (IsString e, MonadError e m, MonadIO m) => ConfigStock -> m ()
invertStock ConfigStock{..} =
  do
    inform $ "Reading vehicle stocks from " ++ show stockSource ++ " . . ."
    stock <- readFieldRecSource stockSource
    inform "Computing vehicle sales . . ."
    let
      sales = inferSales (fromMaybe 0 priorYears) survivalFunction stock
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
