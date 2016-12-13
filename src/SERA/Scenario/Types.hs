-----------------------------------------------------------------------------
--
-- Module      :  SERA.Scenario.Types
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Types for scenario computations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Scenario.Types (
-- * Data cubes
  LogisticCube
, UrbanCharacteristicsCube
, RegionalIntroductionParametersCube
, OverrideIntroductionYearsCube
, RegionalIntroductionsCube
-- * Fields and labels
, FReferenceYear
, fReferenceYear
, FReferenceShare
, fReferenceShare
, FMaximumShare
, fMaximumShare
, FGrowthRate
, fGrowthRate
, FTimeScaling
, fTimeScaling
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
) where


import Data.Daft.Vinyl.FieldCube (type (↝))
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Vinyl.Lens (type (∈))
import SERA.Types (FRegion, FUrbanCode, FUrbanName)
import SERA.Vehicle.Types (MarketShare, ModelYear, FRelativeMarketShare, FStock, FVehicle, FVocation)


-- | Data Cube for logistics parameters
type LogisticCube = '[FRegion, FVocation, FVehicle] ↝ '[FReferenceYear, FReferenceShare, FMaximumShare, FGrowthRate, FTimeScaling]


-- | Data cube for urban characteristics.
type UrbanCharacteristicsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FArea, FPopulation, FPercentileEAM, FNearbyPercentileEAM, FStock]


-- | Data Cube for introduction year parameters.
type RegionalIntroductionParametersCube = '[FRegion] ↝ '[FFirstYear, FLastYear, FClustering, FDelay, FIntensification]


-- | Data cube for which introduction years to override.
type OverrideIntroductionYearsCube = '[FUrbanCode, FUrbanName] ↝ '[FIntroductionYear]


-- | Data cube for regional introductions.
type RegionalIntroductionsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations]


-- | Field type for reference year of a logistic curve.
type FReferenceYear = '("Reference Year", ModelYear)


-- | Field label for reference year of a logistic curve.
fReferenceYear :: SField FReferenceYear
fReferenceYear = SField


-- | Field type for reference share of a logistic curve.
type FReferenceShare = '("Reference Share [veh/veh]", MarketShare)


-- | Field label for reference share of a logistic curve.
fReferenceShare :: SField FReferenceShare
fReferenceShare = SField


-- | Field type for maximum share of a logistic curve.
type FMaximumShare = '("Maximum Share [veh/veh]", MarketShare)


-- | Field label for maximum share of a logistic curve.
fMaximumShare :: SField FMaximumShare
fMaximumShare = SField


-- | Field type for growth rate of a logistic curve.
type FGrowthRate = '("Growth Rate [/yr]", Double)


-- | Field label for growth rate of a logistic curve.
fGrowthRate :: SField FGrowthRate
fGrowthRate = SField


-- | Field type for time scaling of a logistic curve.
type FTimeScaling = '("Time Scaling [yr/yr]", Double)


-- | Field label for time scaling of a logistic curve.
fTimeScaling :: SField FTimeScaling
fTimeScaling = SField


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