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
, calculateIntroductions
) where


import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Source (DataSource(..), withSource)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeSource, writeFieldCubeSource)
import Data.String (IsString)
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Service ()
import SERA.Scenario.Introduction (IntroductionParameters, introductionYears)


-- | Configuration for vehicle stock modeling.
data ConfigIntroduction =
  ConfigIntroduction
  {
    urbanCharacteristicsSource  :: DataSource Void        -- ^ Inputs.
  , regionalIntroductionsSource :: DataSource Void        -- ^ Outputs.
  , parameters   :: IntroductionParameters -- ^ Logit parameters.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigIntroduction

instance ToJSON ConfigIntroduction


-- | Compute introduction years.
calculateIntroductions :: (IsString e, MonadError e m, MonadIO m)
                       => ConfigIntroduction -- ^ Configuration data.
                       -> m ()               -- ^ Action to compute the introduction years.
calculateIntroductions ConfigIntroduction{..} =
  do
    inform $ "Reading urban characteristics from " ++ show urbanCharacteristicsSource ++ " . . ."
    urbanCharacteristics <- readFieldCubeSource urbanCharacteristicsSource
    let
      regionalIntroductions = introductionYears parameters urbanCharacteristics
    withSource regionalIntroductionsSource $ \source -> do
      inform $ "Writing regional introduction years to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source regionalIntroductions
