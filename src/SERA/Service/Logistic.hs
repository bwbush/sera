-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.Logistic
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for logistic scenario generation.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}


module SERA.Service.Logistic (
-- * Configuration
  ConfigLogistic(..)
-- * Computation
, calculateLogistic
) where


import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..), withSource)
import Data.Daft.Vinyl.FieldRec.IO (readFieldRecSource, writeFieldRecSource)
import Data.Daft.Vinyl.FieldCube (fromRecords, toKnownRecords)
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Service ()
import SERA.Scenario.Logistic (LogisticParameters, applyLogistic)


-- | Configuration for vehicle stock modeling.
data ConfigLogistic =
  ConfigLogistic
  {
    totalSalesSource   :: DataSource Void    -- ^ Total vehicle sales.
  , vehicleSalesSource :: DataSource Void    -- ^ Vehicle-specific sales.
  , parameters         :: LogisticParameters -- ^ Logit parameters.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigLogistic

instance ToJSON ConfigLogistic


-- | Compute vehicle stock.
calculateLogistic :: (IsString e, MonadError e m, MonadIO m)
               => ConfigLogistic -- ^ Configuration data.
               -> m ()           -- ^ Action to compute the logistic scenario.
calculateLogistic ConfigLogistic{..} =
  do
    inform $ "Reading total vehicle sales from " ++ show totalSalesSource ++ " . . ."
    sales <- fromRecords <$> readFieldRecSource totalSalesSource
    let
      sales' = applyLogistic parameters sales
    withSource vehicleSalesSource $ \source -> do
      inform $ "Writing vehicle-specific sales to " ++ show source ++ " . . ."
      void . writeFieldRecSource source $ toKnownRecords sales'
