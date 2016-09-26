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
import Data.Daft.Vinyl.FieldCube (type (↝), π, σ)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Default.Util (nan)
import Data.Maybe (fromMaybe)
import Data.Vinyl.Derived (FieldRec, SField(..))
import GHC.Generics (Generic)
import SERA.Types (Region, FRegion, fRegion, FUrbanCode, FUrbanName)
import SERA.Vehicle.Types (FAnnualTravel, fAnnualTravel, FModelYear, fModelYear, FRelativeMarketShare, fRelativeMarketShare, FStock, fStock)


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


type IntroductionInputsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FBaseYear, FNearbyYear, FPercentileEAM, FStock, FAnnualTravel]

type IntroductionOutputsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FRelativeMarketShare, FModelYear]


introducing :: IntroductionParameters -> FieldRec '[FRegion, FUrbanCode, FUrbanName] -> FieldRec '[FBaseYear, FNearbyYear, FPercentileEAM, FStock, FAnnualTravel] -> FieldRec '[FRelativeMarketShare, FModelYear]
introducing IntroductionParameters{..} key rec =
  let
    base = fBaseYear <: rec
    nearby = fNearbyYear <: rec
    eam = fPercentileEAM <: rec / 100
    region = fRegion <: key
    delay = fromMaybe nan $ region `lookup` delays
    stock = fStock <: rec
    shareIntensification = fromMaybe nan $ region `lookup` shareIntensifications
  in
        fRelativeMarketShare =: shareIntensification * stock
    <+> fModelYear           =: round (base * (1 - clustering) + nearby * clustering + delay * eam)


hasAnnualTravel :: FieldRec '[FRegion, FUrbanCode, FUrbanName] -> FieldRec '[FBaseYear, FNearbyYear, FPercentileEAM, FStock, FAnnualTravel] -> Bool
hasAnnualTravel = const $ not . isNaN . (fAnnualTravel <:)

introductionYears :: IntroductionParameters -> IntroductionInputsCube -> IntroductionOutputsCube
introductionYears = (. σ hasAnnualTravel) . π . introducing
