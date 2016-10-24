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
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeSource, writeFieldCubeSource)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Service ()
import SERA.Scenario.Logistic (applyLogistic)
import SERA.Vehicle.Types (ModelYear)


-- | Configuration for vehicle stock modeling.
data ConfigLogistic =
  ConfigLogistic
  {
    logisticSource    :: DataSource Void
  , firstModelYear    :: ModelYear
  , lastModelYear     :: ModelYear
  , overrideShareSource :: DataSource Void
  , marketShareSource :: DataSource Void
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigLogistic

instance ToJSON ConfigLogistic


-- | Compute logistic market shares.
calculateLogistic :: (IsString e, MonadError e m, MonadIO m)
                  => ConfigLogistic -- ^ Configuration data.
                  -> m ()           -- ^ Action to compute the logistic scenario.
calculateLogistic ConfigLogistic{..} =
  do
    inform $ "Reading logistic parameters from " ++ show logisticSource ++ " . . ."
    logistics <- readFieldCubeSource logisticSource
    inform $ "Reading market share overrides from " ++ show overrideShareSource ++ " . . ."
    overrides <- readFieldCubeSource overrideShareSource
    let
      marketShares = overrides <> applyLogistic (firstModelYear, lastModelYear) logistics
    withSource marketShareSource $ \source -> do
      inform $ "Writing vehicle-specific sales to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source marketShares
