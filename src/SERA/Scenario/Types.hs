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


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Scenario.Types (
-- * Data cubes
  LogisticCube
, UrbanCharacteristicsCube
, RegionalIntroductionParametersCube
, OverrideIntroductionYearsCube
, RegionalIntroductionsCube
, GrantsCube

-- * Fields and labels
, FReferenceYear
, fReferenceYear
, FReferenceShare
, fReferenceShare
, FMaximumSales
, fMaximumSales
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
, Cohort(..)
, FCohort
, fCohort
, FGrants
, fGrants
, FInitialGrant
, fInitialGrant
, FAnnualGrant
, fAnnualGrant
, FGrantDuration
, fGrantDuration
, FRollover
, fRollover
) where


import Data.Daft.Vinyl.FieldCube (type (↝))
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import SERA.Types (FRegion, FUrbanCode, FUrbanName, FYear)
import SERA.Types.TH (makeField, makeStringField)
import SERA.Vehicle.Types (MarketShare, ModelYear, FRelativeMarketShare, Sales, FVehicle, FVocation)


$(makeStringField "Cohort" "Cohort")

$(makeField "Grants" "Grants [$]" ''Double)

$(makeField "InitialGrant" "Fixed Grant [$/$]" ''Double)

$(makeField "AnnualGrant" "Operating Grant [$]" ''Double)

$(makeField "GrantDuration" "Grant Duration [yr]" ''Int)

$(makeField "Rollover" "Rollover?" ''Bool)


-- | Field type for reference year of a logistic curve.
-- | Field label for reference year of a logistic curve.
$(makeField "ReferenceYear" "Reference Year" ''ModelYear)


-- | Field type for reference share of a logistic curve.
-- | Field label for reference share of a logistic curve.
$(makeField "ReferenceShare" "Reference Share [veh/veh]" ''MarketShare)


-- | Field type for maximum share of a logistic curve.
-- | Field label for maximum share of a logistic curve.
$(makeField "MaximumSales" "Maximum Sales [veh]" ''Sales)


-- | Field type for maximum share of a logistic curve.
-- | Field label for maximum share of a logistic curve.
$(makeField "MaximumShare" "Maximum Share [veh/veh]" ''MarketShare)


-- | Field type for growth rate of a logistic curve.
-- | Field label for growth rate of a logistic curve.
$(makeField "GrowthRate" "Growth Rate [/yr]" ''Double)


-- | Field type for time scaling of a logistic curve.
-- | Field label for time scaling of a logistic curve.
$(makeField "TimeScaling" "Time Scaling [yr/yr]" ''Double)


-- | Field type for area.
-- | Field label for area.
$(makeField "Area" "Area [km^2]" ''Double)


-- | Field type for population.
-- | Field label for population.
$(makeField "Population" "Population" ''Double)


-- | Field type for EAM percentile.
-- | Field label for EAM percentile.
$(makeField "PercentileEAM" "EAM Percentile" ''Double)


-- | Field type for nearby EAM percentile.
-- | Field label for nearby EAM percentile.
$(makeField "NearbyPercentileEAM" "Nearby EAM Percentile" ''Double)


-- | Field type for first year.
-- | Field label for first year.
$(makeField "FirstYear" "First Year" ''Double)


-- | Field type for last year.
-- | Field label for last year.
$(makeField "LastYear" "Last Year" ''Double)


-- | Field type for clustering parameter.
-- | Field label for clustering parameter.
$(makeField "Clustering" "Clustering" ''Double)


-- | Field type for delay parameter.
-- | Field label for delay parameter.
$(makeField "Delay" "Delay" ''Double)


-- | Field type for intensification parameter.
-- | Field label for intensification parameter.
$(makeField "Intensification" "Intensification" ''Double)


-- | Type for introduction year.
type IntroductionYear = Int


-- | Field type for introduction year.
-- | Field label for introduction year.
$(makeField "IntroductionYear" "Introduction Year" ''IntroductionYear)


-- | Type for station count.
type StationCount = Int


-- | Field type for station count.
-- | Field label for station count.
$(makeField "StationCount" "Station Count" ''StationCount)


-- | Field type for coverage stations.
-- | Field label for coverage stations.
$(makeField "CoverageStations" "Coverage Stations" ''StationCount)


-- | Field type for threshhold stations.
-- | Field type for threshhold stations.
$(makeField "ThreshholdStations" "Threshhold Stations" ''StationCount)


-- | Field type for maximum stations.
-- | Field label for maximum stations.
$(makeField "MaximumStations" "Maximum Stations" ''StationCount)


-- | Determine whether a record has stations.
hasStations :: (FMaximumStations ∈ vs)
            => k           -- ^ The key.
            -> FieldRec vs -- ^ The value.
            -> Bool        -- ^ Whether the value has stations.
hasStations = const $ (/= 0) . (fMaximumStations <:)


-- | Data Cube for logistics parameters
type LogisticCube = '[FRegion, FVocation, FVehicle] ↝ '[FReferenceYear, FReferenceShare, FMaximumShare, FGrowthRate, FTimeScaling]


-- | Data cube for urban characteristics.
type UrbanCharacteristicsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FArea, FPopulation, FPercentileEAM, FNearbyPercentileEAM, FMaximumSales]


-- | Data Cube for introduction year parameters.
type RegionalIntroductionParametersCube = '[FRegion] ↝ '[FFirstYear, FLastYear, FClustering, FDelay, FIntensification]


-- | Data cube for which introduction years to override.
type OverrideIntroductionYearsCube = '[FUrbanCode, FUrbanName] ↝ '[FIntroductionYear]


-- | Data cube for regional introductions.
type RegionalIntroductionsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FMaximumSales, FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations]


-- | Data cube for regional incentives.
type GrantsCube = '[FCohort, FYear] ↝ '[FGrants, FInitialGrant, FAnnualGrant, FGrantDuration, FRollover]
