-----------------------------------------------------------------------------
--
-- Module      :  SERA.Scenario.Introduction
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Computing introduction years for vehicles.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}


module SERA.Scenario.Introduction (
-- * Types
  IntroductionParameters(..)
-- * Functions
, introductionYears
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Vinyl.FieldCube (type (↝), π)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Default.Util (nan)
import Data.Maybe (fromMaybe)
import Data.Vinyl.Derived (FieldRec, SField(..))
import GHC.Generics (Generic)
import SERA.Types (Region, FRegion, fRegion, FUrbanCode, FUrbanName)
import SERA.Vehicle.Types (FModelYear, fModelYear)


data IntroductionParameters =
  IntroductionParameters
  {
    clustering            :: Double
  , delays                :: [(Region, Double)]
  , shareIntensifications :: [(Region, Double)]
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON IntroductionParameters

instance ToJSON IntroductionParameters


type FBaseYear = '("Introduction Year - Base", Double)

fBaseYear :: SField FBaseYear
fBaseYear = SField


type FNearbyYear = '("Nearby Introduction", Double)

fNearbyYear :: SField FNearbyYear
fNearbyYear = SField


type FPercentileEAM = '("EAM Percentile", Double)

fPercentileEAM :: SField FPercentileEAM
fPercentileEAM = SField


type IntroductionInputsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FBaseYear, FNearbyYear, FPercentileEAM]

type IntroductionOutputsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FModelYear]


introducing :: IntroductionParameters -> FieldRec '[FRegion, FUrbanCode, FUrbanName] -> FieldRec '[FBaseYear, FNearbyYear, FPercentileEAM] -> FieldRec '[FModelYear]
introducing IntroductionParameters{..} key rec =
  let
    base = fBaseYear <: rec
    nearby = fNearbyYear <: rec
    eam = fPercentileEAM <: rec / 100
    delay = fromMaybe nan $ fRegion <: key `lookup` delays
  in
    fModelYear =: round (base * (1 - clustering) + nearby * clustering + delay * eam)


introductionYears :: IntroductionParameters -> IntroductionInputsCube -> IntroductionOutputsCube
introductionYears = π . introducing
