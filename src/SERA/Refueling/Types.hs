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
{-# LANGUAGE TupleSections              #-}
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


import Control.Arrow (first)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withText)
import Data.Daft.Vinyl.FieldCube -- (type (↝), π, σ)
import Data.Default (Default(..))
import Data.String.ToString (toString)
import Data.Vinyl.Derived (SField(..))
import GHC.Generics (Generic)
import SERA.Service ()
import SERA.Types -- (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName, FYear, fYear)
import SERA.Vehicle.Types


-- | Data cube for external station capacity.
type GlobalCapacityCube = '[FYear] ↝ '[FTotalCapacity]


-- | Data cube for station details.
type StationDetailCube = '[FRegion, FYear, FStationID] ↝ '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction]


-- | Data cube summarizing stations.
type StationSummaryCube = '[FYear, FRegion] ↝ '[FSales, FStock, FTravel, FEnergy, FDemand, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]


-- | Field type for new capital cost.
type FNewCapitalCost = '("New Capital Cost [$]", Double)


-- | Field label for new capital cost.
fNewCapitalCost :: SField FNewCapitalCost
fNewCapitalCost = SField


-- | Field type for new installation cost.
type FNewInstallationCost = '("New Installation Cost [$]", Double)


-- | Field label for new installation cost.
fNewInstallationCost :: SField FNewInstallationCost
fNewInstallationCost = SField


-- | Field type for new capital incentives.
type FNewCapitalIncentives = '("New Capital Incentives [$]", Double)


-- | Field label for new capital incentives.
fNewCapitalIncentives :: SField FNewCapitalIncentives
fNewCapitalIncentives = SField


-- | Field type for new production incentives.
type FNewProductionIncentives = '("New Production Incentives [$]", Double)


-- | Field label for new production incentives.
fNewProductionIncentives :: SField FNewProductionIncentives
fNewProductionIncentives = SField


-- | Field type for new electrolysis capacity.
type FNewElectrolysisCapacity = '("New Electrolysis [kg/day]", Double)


-- | Field label for new electrolysis capacity.
fNewElectrolysisCapacity :: SField FNewElectrolysisCapacity
fNewElectrolysisCapacity = SField


-- | Field type for new pipeline capacity.
type FNewPipelineCapacity = '("New Pipeline [kg/day]", Double)


-- | Field label for new pipeline capacity.
fNewPipelineCapacity :: SField FNewPipelineCapacity
fNewPipelineCapacity = SField


-- | Field type for new on-site SMR capacity.
type FNewOnSiteSMRCapacity = '("New On-Site SMR [kg/day]", Double)


-- | Field label for new on-site SMR capacity.
fNewOnSiteSMRCapacity :: SField FNewOnSiteSMRCapacity
fNewOnSiteSMRCapacity = SField


-- | Field type for new GH2 truck capacity.
type FNewGH2TruckCapacity = '("New GH2 [kg/day]", Double)


-- | Field label for new GH2 truck capacity.
fNewGH2TruckCapacity :: SField FNewGH2TruckCapacity
fNewGH2TruckCapacity = SField


-- | Field type for new LH2 truck capacity.
type FNewLH2TruckCapacity = '("New LH2 [kg/day]", Double)


-- | Field label for new LH2 truck capacity.
fNewLH2TruckCapacity :: SField FNewLH2TruckCapacity
fNewLH2TruckCapacity = SField


-- | Field type for fraction of capacity from renewable sources.
type FRenewableFraction = '("Renewable Fraction [kg/kg]", Double)


-- | Field label for fraction of capacity from renewable sources.
fRenewableFraction :: SField FRenewableFraction
fRenewableFraction = SField


-- | Field type for demand.
type FDemand = '("Demand [kg/day]", Double)


-- | Field label for demand.
fDemand :: SField FDemand
fDemand = SField


-- | Field type for number of new stations.
type FNewStations = '("New Stations", Int)


-- | Field label for number of new stations.
fNewStations :: SField FNewStations
fNewStations = SField


-- | Field type for total number of stations.
type FTotalStations = '("Total Stations", Int)


-- | Field label for total number of stations.
fTotalStations :: SField FTotalStations
fTotalStations = SField


-- | Field type for capacity of new stations.
type FNewCapacity = '("New Capacity [kg/day]", Double)


-- | Field label for capacity of new stations.
fNewCapacity :: SField FNewCapacity
fNewCapacity = SField


-- | Field type for total capacity.
type FTotalCapacity = '("Total Capacity [kg/day]", Double)


-- | Field label for total capacity.
fTotalCapacity :: SField FTotalCapacity
fTotalCapacity = SField


-- | Type for station identifiers.
newtype StationID = StationID {stationID :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read StationID where
  readsPrec
    | quotedStringTypes = (fmap (first StationID) .) . readsPrec
    | otherwise         = const $ return . (, []) . StationID

instance Show StationID where
  show
    | quotedStringTypes = show . stationID
    | otherwise         = stationID

instance FromJSON StationID where
  parseJSON = withText "SERA.Types.StationID" $ return . StationID . toString

instance ToJSON StationID where
  toJSON = toJSON . stationID


-- | Field type for station identifiers.
type FStationID = '("Station ID", StationID)


-- | Field label for station identifiers.
fStationID :: SField FStationID
fStationID = SField
