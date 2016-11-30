-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.Introduction
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for estimating introduction years.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}


module SERA.Service.Introduction (
-- * Configuration
  ConfigIntroduction(..)
-- * Computation
, introductionsMain
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..))
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (verboseReadFieldCubeSource, verboseWriteFieldCubeSource)
import SERA.Service ()
import SERA.Scenario.Introduction (StationParameters, computeIntroductionYears)


-- | Configuration for computing introduction years.
data ConfigIntroduction =
  ConfigIntroduction
  {
    urbanCharacteristicsSource           :: DataSource Void   -- ^ Source of urban characteristic data.
  , regionalIntroductionParametersSource :: DataSource Void   -- ^ Source of introduction year parameters.
  , overrideIntroductionYearsSource      :: DataSource Void   -- ^ Source of which introduction years to override.
  , regionalIntroductionsSource          :: DataSource Void   -- ^ Source for regional introductions.
  , stationParameters                    :: StationParameters -- ^ Logit parameters.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigIntroduction

instance ToJSON ConfigIntroduction


-- | Compute introduction years.
introductionsMain :: (IsString e, MonadError e m, MonadIO m)
                       => ConfigIntroduction -- ^ Configuration data.
                       -> m ()               -- ^ Action to compute the introduction years.
introductionsMain ConfigIntroduction{..} =
  do
    urbanCharacteristics <- verboseReadFieldCubeSource "urban characteristics"            urbanCharacteristicsSource
    regionalParameters   <- verboseReadFieldCubeSource "regional introduction parameters" regionalIntroductionParametersSource
    overrides            <- verboseReadFieldCubeSource "overrides for introduction years" overrideIntroductionYearsSource
    let
      regionalIntroductions = computeIntroductionYears stationParameters overrides regionalParameters urbanCharacteristics
    verboseWriteFieldCubeSource "regional introduction years" regionalIntroductionsSource regionalIntroductions
