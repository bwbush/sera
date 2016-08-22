{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module SERA.Service.VehicleStock (
-- * Configuration
  ConfigInvertStock(..)
, SurvivalData(..)
-- * Computation
, computeStock
, invertStock
) where


import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..), maybeWithSource)
import Data.Daft.Vinyl.FieldRec (readFieldRecSource, writeFieldRecSource)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Service ()
import SERA.Vehicle.Stock (inferMarketShares, inferSales)
import VISION.Survival (survivalFunction)


data SurvivalData = VISION_LDV | VISION_MHDV
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON SurvivalData

instance ToJSON SurvivalData

instance Default SurvivalData where
  def = VISION_LDV


computeStock :: (IsString e, MonadError e m, MonadIO m) => ConfigInvertStock -> m ()
computeStock ConfigInvertStock{..} =
  undefined


data ConfigInvertStock =
  ConfigInvertStock
  {
    stockSource         :: DataSource Void
  , salesStockSource    :: Maybe (DataSource Void)
  , regionalSalesSource :: Maybe (DataSource Void)
  , marketSharesSource  :: Maybe (DataSource Void)
  , survivalSource      :: Maybe (DataSource SurvivalData)
  , priorYears          :: Maybe Int
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigInvertStock

instance ToJSON ConfigInvertStock


invertStock :: (IsString e, MonadError e m, MonadIO m) => ConfigInvertStock -> m ()
invertStock ConfigInvertStock{..} =
  do
    inform $ "Reading vehicle stocks from " ++ show stockSource ++ " . . ."
    stock <- readFieldRecSource stockSource
    inform "Computing vehicle sales . . ."
    let
      sales = inferSales (fromMaybe 0 priorYears) survivalFunction stock
      (regionalSales, shares) = inferMarketShares sales
    maybeWithSource salesStockSource $ \source -> do
      inform $ "Writing vehicle sales and stocks to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source sales
    maybeWithSource regionalSalesSource $ \source -> do
      inform $ "Writing regional sales to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source regionalSales
    maybeWithSource marketSharesSource $ \source -> do
      inform $ "Writing market shares to " ++ show source ++ " . . ."
      void $ writeFieldRecSource source shares
