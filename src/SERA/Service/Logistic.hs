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
, logisticMain
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..))
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (verboseReadFieldCubeSource, verboseWriteFieldCubeSource)
import SERA.Scenario.Logistic (computeMarketShares)
import SERA.Service ()
import SERA.Types.Fields (ModelYear)


-- | Configuration for logistic curve computations.
data ConfigLogistic =
  ConfigLogistic
  {
    logisticSource      :: DataSource Void -- ^ Source of logistic parameters.
  , firstModelYear      :: ModelYear       -- ^ The first model year to compute.
  , lastModelYear       :: ModelYear       -- ^ The last moodel year to compute.
  , overrideShareSource :: DataSource Void -- ^ Source of which market shares to manually override.
  , marketShareSource   :: DataSource Void -- ^ Source for the market shares.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigLogistic

instance ToJSON ConfigLogistic


-- | Compute logistic market shares.
logisticMain :: (IsString e, MonadError e m, MonadIO m)
                  => ConfigLogistic -- ^ Configuration data.
                  -> m ()           -- ^ Action to compute the logistic scenario.
logisticMain ConfigLogistic{..} =
  do
    logistics <- verboseReadFieldCubeSource "logistic parameters" logisticSource
    overrides <- verboseReadFieldCubeSource "market share"        overrideShareSource
    let
      marketShares = overrides <> computeMarketShares (firstModelYear, lastModelYear) logistics
    verboseWriteFieldCubeSource "vehicle-specific sales" marketShareSource marketShares
