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


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Scenario.Introduction (
-- * Types
  StationParameters(..)
, CoverageParameters(..)
, StationCountParameters(..)
, UrbanCharacteristicsCube
, RegionalIntroductionParametersCube
, OverrideIntroductionYearsCube
, RegionalIntroductionsCube
-- * Fields and labels
, FArea
, fArea
, FPopulation
, fPopulation
, FPercentileEAM
, fPercentileEAM
, FNearbyPercentileEAM
, fNearbyPercentileEAM
, FFirstYear
, fFirstYear
, FLastYear
, fLastYear
, FClustering
, fClustering
, FDelay
, fDelay
, FIntensification
, fIntensification
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
, hasStations
-- * Functions
, computeIntroductionYears
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldCube (type (↝), (⋈), π, τ)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Vinyl.Lens (type (∈))
import GHC.Generics (Generic)
import SERA.Types (FRegion, FUrbanCode, FUrbanName)
import SERA.Vehicle.Types (FRelativeMarketShare, fRelativeMarketShare, FStock, fStock)


-- | Data cube for urban characteristics.
type UrbanCharacteristicsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FArea, FPopulation, FPercentileEAM, FNearbyPercentileEAM, FStock]


-- | Data Cube for introduction year parameters.
type RegionalIntroductionParametersCube = '[FRegion] ↝ '[FFirstYear, FLastYear, FClustering, FDelay, FIntensification]


-- | Data cube for which introduction years to override.
type OverrideIntroductionYearsCube = '[FUrbanCode, FUrbanName] ↝ '[FIntroductionYear]


-- | Data cube for regional introductions.
type RegionalIntroductionsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations]


-- | Field type for area.
type FArea = '("Area [km^2]", Double)


-- | Field label for area.
fArea :: SField FArea
fArea = SField


-- | Field type for population.
type FPopulation = '("Population", Double)


-- | Field label for population.
fPopulation :: SField FPopulation
fPopulation = SField


-- | Field type for EAM percentile.
type FPercentileEAM = '("EAM Percentile", Double)


-- | Field label for EAM percentile.
fPercentileEAM :: SField FPercentileEAM
fPercentileEAM = SField


-- | Field type for nearby EAM percentile.
type FNearbyPercentileEAM = '("Nearby EAM Percentile", Double)


-- | Field label for nearby EAM percentile.
fNearbyPercentileEAM :: SField FNearbyPercentileEAM
fNearbyPercentileEAM = SField


-- | Field type for first year.
type FFirstYear = '("First Year", Double)


-- | Field label for first year.
fFirstYear :: SField FFirstYear
fFirstYear = SField


-- | Field type for last year.
type FLastYear = '("Last Year", Double)


-- | Field label for last year.
fLastYear :: SField FLastYear
fLastYear = SField


-- | Field type for clustering parameter.
type FClustering = '("Clustering", Double)


-- | Field label for clustering parameter.
fClustering :: SField FClustering
fClustering = SField


-- | Field type for delay parameter.
type FDelay = '("Delay", Double)


-- | Field label for delay parameter.
fDelay :: SField FDelay
fDelay = SField


-- | Field type for intensification parameter.
type FIntensification = '("Intensification", Double)


-- | Field label for intensification parameter.
fIntensification :: SField FIntensification
fIntensification = SField


-- | Type for introduction year.
type IntroductionYear = Int


-- | Field type for introduction year.
type FIntroductionYear = '("Introduction Year", IntroductionYear)


-- | Field label for introduction year.
fIntroductionYear :: SField FIntroductionYear
fIntroductionYear = SField


-- | Type for station count.
type StationCount = Int


-- | Field type for station count.
type FStationCount = '("Station Count", StationCount)


-- | Field label for station count.
fStationCount :: SField FStationCount
fStationCount = SField


-- | Field type for coverage stations.
type FCoverageStations = '("Coverage Stations", StationCount)


-- | Field label for coverage stations.
fCoverageStations :: SField FCoverageStations
fCoverageStations = SField


-- | Field type for threshhold stations.
type FThreshholdStations = '("Threshhold Stations", StationCount)


-- | Field type for threshhold stations.
fThreshholdStations :: SField FThreshholdStations
fThreshholdStations = SField


-- | Field type for maximum stations.
type FMaximumStations = '("Maximum Stations", StationCount)


-- | Field label for maximum stations.
fMaximumStations :: SField FMaximumStations
fMaximumStations = SField


-- | Determine whether a record has stations.
hasStations :: (FMaximumStations ∈ vs)
            => k           -- ^ The key.
            -> FieldRec vs -- ^ The value.
            -> Bool        -- ^ Whether the value has stations.
hasStations = const $ (/= 0) . (fMaximumStations <:)


-- | Parameters for computing number of stations for an urban area.
data StationParameters =
  StationParameters
  {
    coverageParameters           :: CoverageParameters     -- ^ Parameters for number of coverage stations.
  , threshholdStationsParameters :: StationCountParameters -- ^ Parameters for number of threshhold stations.
  , maximumStationsParameters    :: StationCountParameters -- ^ Parameters for maximum number of stations.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON StationParameters

instance ToJSON StationParameters


-- | Parameters for number of coverage stations.
data CoverageParameters =
  CoverageParameters
  {
    scaleFactor   :: Double -- ^ Scale factor.
  , densityFactor :: Double -- ^ Density factor.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON CoverageParameters

instance ToJSON CoverageParameters


-- | Estimate number of coverage stations.
coverageCount :: CoverageParameters -- ^ Coverage station parameters.
              -> Double             -- ^ Population density, per square mile.
              -> Double             -- ^ Area, in square miles.
              -> Double             -- ^ Number of coverage stations.
coverageCount CoverageParameters{..} density area =
  1 +  scaleFactor * exp(- densityFactor * density) * (area / 100)


-- | Parameters for number of stations.
data StationCountParameters =
  StationCountParameters
  {
    intercept :: Double -- ^ Intercept. 
  , slope     :: Double -- ^ Slope.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON StationCountParameters

instance ToJSON StationCountParameters


-- | Estimate number of stations.
stationCount :: StationCountParameters -- ^ Station count parameters.
              -> Double                -- ^ Population.
              -> Double                -- ^ Number of stations.
stationCount StationCountParameters{..} population =
  slope * population / 1000000 + intercept


-- | Compute introduction years.
computeIntroductionYears :: StationParameters                  -- ^ The station count parameters.
                         -> OverrideIntroductionYearsCube      -- ^ Which introduction years to override.
                         -> RegionalIntroductionParametersCube -- ^ Parameters for regional introdutions.
                         -> UrbanCharacteristicsCube           -- ^ Urban characteristics.
                         -> RegionalIntroductionsCube          -- ^ Regional introduction years.
computeIntroductionYears StationParameters{..} overrides regional urban =
  let
    introducing key rec =
      let
        introduce percentile = fFirstYear <: rec + (fLastYear <: rec - fFirstYear <: rec) * percentile / 100
        eam = fPercentileEAM <: rec / 100
        base = introduce $ fPercentileEAM <: rec
        nearby = introduce $ fNearbyPercentileEAM <: rec
        area = fArea <: rec / 1.60934^(2 :: Int)
        population = fPopulation <: rec
        density = population / area
        coverageStations = coverageCount coverageParameters density area
        clustering = fClustering <: rec
        delay = fDelay <: rec
        floor'   x = if isNaN population then 0 else floor   x
        ceiling' x = if isNaN population then 0 else ceiling x
      in
            fRelativeMarketShare =: fIntensification <: rec * fStock <: rec
        <+> fIntroductionYear    =: maybe
                                       (round (base * (1 - clustering) + nearby * clustering + delay * eam))
                                       (fIntroductionYear <:)
                                       (overrides `evaluate` τ key)
        <+> fStationCount        =: floor' (coverageStations / 3 + 1)
        <+> fCoverageStations    =: ceiling' coverageStations
        <+> fThreshholdStations  =: ceiling' (stationCount threshholdStationsParameters population)
        <+> fMaximumStations     =: ceiling' (stationCount maximumStationsParameters    population)
  in
    π introducing
      $ regional ⋈ urban
