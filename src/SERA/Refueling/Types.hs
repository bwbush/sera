-----------------------------------------------------------------------------
--
-- Module      :  SERA.Refueling.Types
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Hydrogen station size computations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Refueling.Types (
-- * Data cubes
  GlobalCapacityCube
, StationDetailCube
, StationSummaryCube
-- * Fields and labels
, FNewCapitalCost
, fNewCapitalCost
, FNewInstallationCost
, fNewInstallationCost
, FNewCapitalIncentives
, fNewCapitalIncentives
, FNewProductionIncentives
, fNewProductionIncentives
, FNewElectrolysisCapacity
, fNewElectrolysisCapacity
, FNewPipelineCapacity
, fNewPipelineCapacity
, FNewOnSiteSMRCapacity
, fNewOnSiteSMRCapacity
, FNewGH2TruckCapacity
, fNewGH2TruckCapacity
, FNewLH2TruckCapacity
, fNewLH2TruckCapacity
, FRenewableFraction
, fRenewableFraction
, FDemand
, fDemand
, FNewStations
, fNewStations
, FTotalStations
, fTotalStations
, FNewCapacity
, fNewCapacity
, FTotalCapacity
, fTotalCapacity
, StationID(..)
, FStationID
, fStationID
) where


import Data.Daft.Vinyl.FieldCube -- (type (↝), π, σ)
import SERA.Service ()
import SERA.Types.Fields hiding (FSales)  -- (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName, FYear, fYear)
import SERA.Types.TH (makeField, makeStringField)
import SERA.Vehicle.Types


-- | Field type for new capital cost.
-- | Field label for new capital cost.
$(makeField "NewCapitalCost" "New Capital Cost [$]" ''Double)


-- | Field type for new installation cost.
-- | Field label for new installation cost.
$(makeField "NewInstallationCost" "New Installation Cost [$]" ''Double)


-- | Field type for new capital incentives.
-- | Field label for new capital incentives.
$(makeField "NewCapitalIncentives" "New Capital Incentives [$]" ''Double)


-- | Field type for new production incentives.
-- | Field label for new production incentives.
$(makeField "NewProductionIncentives" "New Production Incentives [$]" ''Double)


-- | Field type for new electrolysis capacity.
-- | Field label for new electrolysis capacity.
$(makeField "NewElectrolysisCapacity" "New Electrolysis [kg/day]" ''Double)


-- | Field type for new pipeline capacity.
-- | Field label for new pipeline capacity.
$(makeField "NewPipelineCapacity" "New Pipeline [kg/day]" ''Double)


-- | Field type for new on-site SMR capacity.
-- | Field label for new on-site SMR capacity.
$(makeField "NewOnSiteSMRCapacity" "New On-Site SMR [kg/day]" ''Double)


-- | Field type for new GH2 truck capacity.
-- | Field label for new GH2 truck capacity.
$(makeField "NewGH2TruckCapacity" "New GH2 [kg/day]" ''Double)


-- | Field type for new LH2 truck capacity.
-- | Field label for new LH2 truck capacity.
$(makeField "NewLH2TruckCapacity" "New LH2 [kg/day]" ''Double)


-- | Field type for fraction of capacity from renewable sources.
-- | Field label for fraction of capacity from renewable sources.
$(makeField "RenewableFraction" "Renewable Fraction [kg/kg]" ''Double)


-- | Field type for demand.
-- | Field label for demand.
$(makeField "Demand" "Demand [kg/day]" ''Double)


-- | Field type for number of new stations.
-- | Field label for number of new stations.
$(makeField "NewStations" "New Stations" ''Int)


-- | Field type for total number of stations.
-- | Field label for total number of stations.
$(makeField "TotalStations" "Total Stations" ''Int)


-- | Field type for capacity of new stations.
-- | Field label for capacity of new stations.
$(makeField "NewCapacity" "New Capacity [kg/day]" ''Double)


-- | Field type for total capacity.
-- | Field label for total capacity.
$(makeField "TotalCapacity" "Total Capacity [kg/day]" ''Double)


-- | Type for station identifiers.
-- | Field type for station identifiers.
-- | Field label for station identifiers.
$(makeStringField "StationID" "Station ID")


-- | Data cube for external station capacity.
type GlobalCapacityCube = '[FYear] ↝ '[FTotalCapacity]


-- | Data cube for station details.
type StationDetailCube = '[FRegion, FYear, FStationID] ↝ '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction]


-- | Data cube summarizing stations.
type StationSummaryCube = '[FYear, FRegion] ↝ '[FSales, FStock, FTravel, FEnergy, FDemand, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]
