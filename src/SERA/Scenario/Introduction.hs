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
  StationParameters(..)
, UrbanCharacteristicsCube
, RegionalIntroductionsCube
, IntroductionYear
, FIntroductionYear
, fIntroductionYear
, StationCount
, FStationCount
, fStationCount
, FCoverageStations
, fCoverageStations
, FThreshholdStations
, fThreshholdStations
, FMaximumStations
, fMaximumStations
-- * Functions
, introductionYears
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldCube (type (↝), (⋈), π, τ)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Vinyl.Derived (FieldRec, SField(..))
import GHC.Generics (Generic)
import SERA.Types (FRegion, FUrbanCode, FUrbanName)
import SERA.Vehicle.Types (FRelativeMarketShare, fRelativeMarketShare)


data StationParameters =
  StationParameters
  {
    coverageParameters           :: CoverageParameters
  , threshholdStationsParameters :: StationCountParameters
  , maximumStationsParameters    :: StationCountParameters
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON StationParameters

instance ToJSON StationParameters


data CoverageParameters =
  CoverageParameters
  {
    scaleFactor   :: Double
  , densityFactor :: Double
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON CoverageParameters

instance ToJSON CoverageParameters


data StationCountParameters =
  StationCountParameters
  {
    intercept :: Double
  , slope     :: Double
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON StationCountParameters

instance ToJSON StationCountParameters


type FFirstYear = '("First Year", Double)

fFirstYear :: SField FFirstYear
fFirstYear = SField


type FLastYear = '("Last Year", Double)

fLastYear :: SField FLastYear
fLastYear = SField


type FClustering = '("Clustering", Double)

fClustering :: SField FClustering
fClustering = SField


type FDelay = '("Delay", Double)

fDelay :: SField FDelay
fDelay = SField


type FIntensification = '("Intensification", Double)

fIntensification :: SField FIntensification
fIntensification = SField


type RegionalIntroductionParametersCube = '[FRegion] ↝ '[FFirstYear, FLastYear, FClustering, FDelay, FIntensification]


type IntroductionYear = Int

type FIntroductionYear = '("Introduction Year", IntroductionYear)

fIntroductionYear :: SField FIntroductionYear
fIntroductionYear = SField


type StationCount = Int

type FStationCount = '("Station Count", StationCount)

fStationCount :: SField FStationCount
fStationCount = SField


type Area = Double

type FArea = '("Area [km^2]", Area)

fArea :: SField FArea
fArea = SField


type Population = Double

type FPopulation = '("Population", Population)

fPopulation :: SField FPopulation
fPopulation = SField


type FPercentileEAM = '("EAM Percentile", Double)

fPercentileEAM :: SField FPercentileEAM
fPercentileEAM = SField


type FNearbyPercentileEAM = '("Nearby EAM Percentile", Double)

fNearbyPercentileEAM :: SField FNearbyPercentileEAM
fNearbyPercentileEAM = SField


type FStock = '("Vehicles", Double)

fStock :: SField FStock
fStock = SField


type UrbanCharacteristicsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FArea, FPopulation, FPercentileEAM, FNearbyPercentileEAM, FStock]


type FCoverageStations = '("Coverage Stations", StationCount)

fCoverageStations :: SField FCoverageStations
fCoverageStations = SField


type FThreshholdStations = '("Threshhold Stations", StationCount)

fThreshholdStations :: SField FThreshholdStations
fThreshholdStations = SField


type FMaximumStations = '("Maximum Stations", StationCount)

fMaximumStations :: SField FMaximumStations
fMaximumStations = SField


type RegionalIntroductionsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations]


type OverrideIntroductionYearsCube = '[FUrbanCode, FUrbanName] ↝ '[FIntroductionYear]


introducing :: StationParameters -> OverrideIntroductionYearsCube -> FieldRec '[FRegion, FUrbanCode, FUrbanName] -> FieldRec '[FFirstYear, FLastYear, FClustering, FDelay, FIntensification, FArea, FPopulation, FPercentileEAM, FNearbyPercentileEAM, FStock] -> FieldRec '[FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations]
introducing StationParameters{..} overrides key rec =
  let
    firstYear = fFirstYear <: rec
    lastYear = fLastYear <: rec
    introduce percentile = firstYear + (lastYear - firstYear) * percentile / 100
    eam = fPercentileEAM <: rec / 100
    base = introduce $ fPercentileEAM <: rec
    nearby = introduce $ fNearbyPercentileEAM <: rec
    stock = fStock <: rec
    area = fArea <: rec / 1.60934^(2 :: Int)
    population = fPopulation <: rec
    density = population / area
    CoverageParameters{..} = coverageParameters
    coverageStations = 1 +  scaleFactor * exp(- densityFactor * density) * (area / 100)
    maximumStations = slope maximumStationsParameters * population / 1000000 + intercept maximumStationsParameters
    threshholdStations = slope threshholdStationsParameters * population / 1000000 + intercept threshholdStationsParameters
    intensification = fIntensification <: rec
    clustering = fClustering <: rec
    delay = fDelay <: rec
    floor' x = if isNaN population then 0 else floor x
    ceiling' x = if isNaN population then 0 else ceiling x
  in
        fRelativeMarketShare =: intensification * stock
    <+> fIntroductionYear    =: maybe
                                   (round (base * (1 - clustering) + nearby * clustering + delay * eam))
                                   (fIntroductionYear <:)
                                   (overrides `evaluate` τ key)
    <+> fStationCount        =: floor' (coverageStations / 3 + 1)
    <+> fCoverageStations    =: ceiling' coverageStations
    <+> fThreshholdStations  =: ceiling' threshholdStations
    <+> fMaximumStations     =: ceiling' maximumStations


introductionYears :: StationParameters -> OverrideIntroductionYearsCube -> RegionalIntroductionParametersCube -> UrbanCharacteristicsCube -> RegionalIntroductionsCube
introductionYears global overrides regional urban =
  π (introducing global overrides)
    $ regional ⋈ urban
