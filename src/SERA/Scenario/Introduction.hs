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
, UrbanCharacteristicsCube
, RegionalIntroductionsCube
, IntroductionYear
, FIntroductionYear
, fIntroductionYear
, StationCount
, FStationCount
, fStationCount
-- * Functions
, introductionYears
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.DataCube (Rekeyer(..), rekey)
import Data.Daft.Vinyl.FieldCube (type (↝), π, σ)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Default.Util (nan)
import Data.Maybe (fromMaybe)
import Data.Vinyl.Derived (FieldRec, SField(..))
import GHC.Generics (Generic)
import SERA.Types (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName)
import SERA.Vehicle.Types (FAnnualTravel, fAnnualTravel, FRelativeMarketShare, fRelativeMarketShare, FStock, fStock)


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


type FNearbyYear = '("Nearby Introduction Year", Double)

fNearbyYear :: SField FNearbyYear
fNearbyYear = SField


type FPercentileEAM = '("EAM Percentile", Double)

fPercentileEAM :: SField FPercentileEAM
fPercentileEAM = SField


type IntroductionYear = Int

type FIntroductionYear = '("Introduction Year", IntroductionYear)

fIntroductionYear :: SField FIntroductionYear
fIntroductionYear = SField


type StationCount = Int

type FStationCount = '("Station Count", StationCount)

fStationCount :: SField FStationCount
fStationCount = SField


type Population = Double

type FPopulation = '("Population", Population)

fPopulation :: SField FPopulation
fPopulation = SField


type Area = Double

type FArea = '("Area [km^2]", Area)

fArea :: SField FArea
fArea = SField


type UrbanCharacteristicsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FArea, FPopulation, FPercentileEAM, FStock, FAnnualTravel, FIntroductionYear, FNearbyYear]

type RegionalIntroductionsCube = '[FRegion] ↝ '[FRelativeMarketShare, FIntroductionYear, FStationCount]


introducing :: IntroductionParameters -> FieldRec '[FRegion, FUrbanCode, FUrbanName] -> FieldRec '[FArea, FPopulation, FPercentileEAM, FStock, FAnnualTravel, FIntroductionYear, FNearbyYear] -> FieldRec '[FRelativeMarketShare, FIntroductionYear, FStationCount]
introducing IntroductionParameters{..} key rec =
  let
    base = fromIntegral $ fIntroductionYear <: rec
    nearby = fNearbyYear <: rec
    eam = fPercentileEAM <: rec / 100
    region = fRegion <: key
    delay = fromMaybe nan $ region `lookup` delays
    stock = fStock <: rec
    shareIntensification = fromMaybe nan $ region `lookup` shareIntensifications
    area = fArea <: rec / 1.60934^(2 :: Int)
    popDens = fPopulation <: rec / area
    coverageStations = 1 +  8.1503640 * exp(-0.0003241 * popDens) * (area / 100)
  in
        fRelativeMarketShare =: shareIntensification * stock
    <+> fIntroductionYear    =: round (base * (1 - clustering) + nearby * clustering + delay * eam)
    <+> fStationCount        =: floor (coverageStations / 3 + 1)


hasAnnualTravel :: FieldRec '[FRegion, FUrbanCode, FUrbanName] -> FieldRec '[FArea, FPopulation, FPercentileEAM, FStock, FAnnualTravel, FIntroductionYear, FNearbyYear] -> Bool
hasAnnualTravel = const $ not . isNaN . (fAnnualTravel <:)


urbanToRegion :: '[FRegion, FUrbanCode, FUrbanName] ↝ v -> '[FRegion] ↝ v
urbanToRegion =
  rekey
    $ Rekeyer
        (\key -> fRegion =: Region (region (fRegion <: key) ++ " | " ++ urbanCode (fUrbanCode <: key) ++ " | " ++ urbanName (fUrbanName <: key)))
        undefined


introductionYears :: IntroductionParameters -> UrbanCharacteristicsCube -> RegionalIntroductionsCube
introductionYears =
  (. σ hasAnnualTravel)
    . (urbanToRegion .)
    . π
    . introducing
