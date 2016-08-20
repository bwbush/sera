{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module SERA.Service.VehicleStock (
-- * Configuration
  ConfigStock(..)
, ConfigSurvival(..)
-- * Computation
, computeStock
, invertStock
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), Value(String), withText)
import Data.Daft.Vinyl.FieldRec (readFieldRecFile, writeFieldRecFile)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (pack, unpack)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Vehicle.Stock (inferMarketShares, inferSales)
import VISION.Survival (survivalFunction)


data ConfigSurvival =
    VisionSurvival
  | SurvivalFile FilePath
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigSurvival where
  parseJSON = withText "ConfigSurvival" $ \x -> case x of
                                                  "VISION" -> return VisionSurvival
                                                  filePath -> return . SurvivalFile $ unpack filePath

instance ToJSON ConfigSurvival where
  toJSON VisionSurvival          = String "VISION"
  toJSON (SurvivalFile filePath) = String . pack $ "FILE " ++ filePath


data ConfigStock =
  ConfigStock
  {
    stockFile         :: FilePath
  , salesStockFile    :: FilePath
  , regionalSalesFile :: FilePath
  , marketSharesFile  :: FilePath
  , survival          :: Maybe ConfigSurvival 
  , priorYears        :: Maybe Int
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigStock

instance ToJSON ConfigStock


computeStock :: (IsString e, MonadError e m, MonadIO m) => ConfigStock -> m ()
computeStock ConfigStock{..} =
  undefined


invertStock :: (IsString e, MonadError e m, MonadIO m) => ConfigStock -> m ()
invertStock ConfigStock{..} =
  do
    inform $ "Reading vehicle stocks from " ++ show stockFile ++ " . . ."
    stock <- readFieldRecFile stockFile
    inform "Computing vehicle sales . . ."
    let
      sales = inferSales (fromMaybe 0 priorYears) survivalFunction stock
      (regionalSales, shares) = inferMarketShares sales
    inform $ "Writing vehicle sales and stocks to " ++ show salesStockFile ++ " . . ."
    writeFieldRecFile salesStockFile sales
    inform $ "Writing regional sales to " ++ show regionalSalesFile ++ " . . ."
    writeFieldRecFile regionalSalesFile regionalSales
    inform $ "Writing market shares to " ++ show marketSharesFile ++ " . . ."
    writeFieldRecFile marketSharesFile shares
