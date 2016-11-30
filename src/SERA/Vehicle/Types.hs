-----------------------------------------------------------------------------
--
-- Module      :  SERA.Vehicle.Types
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Types for vehicle modeling.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Vehicle.Types (
-- * Vehicle vocation
  Vocation(..)
, FVocation
, fVocation
-- * Vehicle type
, Vehicle(..)
, FVehicle
, fVehicle
-- * Age
, Age
, FAge
, fAge
-- * Model year
, ModelYear
, FModelYear
, fModelYear
-- * Fuel type
, Fuel(..)
, FFuel
, fFuel
-- * Pollutant
, Pollutant
, FPollutant
, fPollutant
-- * Market share
, MarketShare
, FMarketShare
, fMarketShare
, RelativeMarketShare
, FRelativeMarketShare
, fRelativeMarketShare
-- * Vehicle survival
, Survival
, FSurvival
, fSurvival
-- * Annual travel
, AnnualTravel
, FAnnualTravel
, fAnnualTravel
-- * Fuel split
, FuelSplit
, FFuelSplit
, fFuelSplit
-- * Fuel efficiency
, FuelEfficiency
, FFuelEfficiency
, fFuelEfficiency
-- * Emission rate
, EmissionRate
, FEmissionRate
, fEmissionRate
-- * Vehicle sales
, Sales
, FSales
, fSales
-- * Vehicle stock
, Stock
, FStock
, fStock
, hasStock
-- * Distance traveled
, Travel
, FTravel
, fTravel
-- * Energy consumed
, Energy
, FEnergy
, fEnergy
-- * Pollutants emitted
, Emission
, FEmission
, fEmission
) where


import Control.Arrow (first)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withText)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Default (Default)
import Data.String.ToString (toString)
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Vinyl.Lens (type (∈))
import GHC.Generics (Generic)
import SERA.Types (quotedStringTypes)


-- | Data type for vehicle vocation.
newtype Vocation = Vocation {vocation :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Vocation where
  readsPrec
    | quotedStringTypes = (fmap (first Vocation) .) . readsPrec
    | otherwise         = const $ return . (, []) . Vocation

instance Show Vocation where
  show
    | quotedStringTypes = show . vocation
    | otherwise         = vocation

instance FromJSON Vocation where
  parseJSON = withText "SERA.Vehicle.Types.Vocation" $ return . Vocation . toString

instance ToJSON Vocation where
  toJSON = toJSON . vocation


-- | Field type for vehicle vocation.
type FVocation = '("Vocation", Vocation)


-- | Field label for vehicle vocation.
fVocation :: SField FVocation
fVocation = SField


-- | Data type for vehicle type.
newtype Vehicle = Vehicle {vehicle :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Vehicle where
  readsPrec
    | quotedStringTypes = (fmap (first Vehicle) .) . readsPrec
    | otherwise         = const $ return . (, []) . Vehicle

instance Show Vehicle where
  show
    | quotedStringTypes = show . vehicle
    | otherwise         = vehicle

instance FromJSON Vehicle where
  parseJSON = withText "SERA.Vehicle.Types.Vehicle" $ return . Vehicle . toString

instance ToJSON Vehicle where
  toJSON = toJSON . vehicle


-- | Field type for vehicle type.
type FVehicle = '("Vehicle", Vehicle)


-- | Field label for vehicle type.
fVehicle :: SField FVehicle
fVehicle = SField


-- | Data type for vehicle age.
type Age = Int


-- | Field type for vehicle age.
type FAge = '("Age [yr]", Age)


-- | Field label for vehicle age.
fAge :: SField FAge
fAge = SField


-- | Data type for model year.
type ModelYear = Int


-- | Field type for model year.
type FModelYear = '("Model Year", ModelYear)


-- | Field label for model year.
fModelYear :: SField FModelYear
fModelYear = SField


-- | Data type for fuel.
newtype Fuel = Fuel {fuel :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Fuel where
  readsPrec
    | quotedStringTypes = (fmap (first Fuel) .) . readsPrec
    | otherwise         = const $ return . (, []) . Fuel

instance Show Fuel where
  show
    | quotedStringTypes = show . fuel
    | otherwise         = fuel

instance FromJSON Fuel where
  parseJSON = withText "SERA.Vehicle.Types.Fuel" $ return . Fuel . toString

instance ToJSON Fuel where
  toJSON = toJSON . fuel


-- | Field type for fuel.
type FFuel = '("Fuel", Fuel)


-- | Field label for fuel.
fFuel :: SField FFuel
fFuel = SField


-- | Data type for pollutant type.
newtype Pollutant = Pollutant {emission :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Pollutant where
  readsPrec
    | quotedStringTypes = (fmap (first Pollutant) .) . readsPrec
    | otherwise         = const $ return . (, []) . Pollutant

instance Show Pollutant where
  show
    | quotedStringTypes = show . emission
    | otherwise         = emission

instance FromJSON Pollutant where
  parseJSON = withText "SERA.Vehicle.Types.Pollutant" $ return . Pollutant . toString

instance ToJSON Pollutant where
  toJSON = toJSON . emission


-- | Field type for pollutant type.
type FPollutant = '("Pollutant", Pollutant)


-- | Field label for pollutant type.
fPollutant :: SField Pollutant
fPollutant = SField


-- | Data type for market share.
type MarketShare = Double


-- | Field type for market share.
type FMarketShare = '("Market Share [veh/veh]", MarketShare)


-- | Field label for market share.
fMarketShare :: SField FMarketShare
fMarketShare = SField


-- | Data type for relative market share.
type RelativeMarketShare = Double


-- | Field type for relative market share.
type FRelativeMarketShare = '("Relative Market Share", RelativeMarketShare)


-- | Field label for relative market share.
fRelativeMarketShare :: SField FRelativeMarketShare
fRelativeMarketShare = SField


-- | Data type for vehicle survival.
type Survival = Double


-- | Field type for vehicle survival.
type FSurvival = '("Surviving Vehicles [veh/veh]", Survival)


-- | Field label for vehicle survival.
fSurvival :: SField FSurvival
fSurvival = SField


-- | Data type for annual travel.
type AnnualTravel = Double


-- | Field type for annual travel.
type FAnnualTravel = '("Annual Travel [mi/yr]", AnnualTravel)


-- | Field label for annual travel.
fAnnualTravel :: SField FAnnualTravel
fAnnualTravel = SField


-- | Data type for fuel split.
type FuelSplit = Double


-- | Field type for fuel split.
type FFuelSplit = '("Fraction of Travel [mi/mi]", FuelSplit)


-- | Field label for fuel split.
fFuelSplit :: SField FFuelSplit
fFuelSplit = SField


-- | Data type for fuel efficiency.
type FuelEfficiency = Double


-- | Field type for fuel efficiency.
type FFuelEfficiency = '("Fuel Efficiency [mi/gge]", FuelEfficiency)


-- | Field label for fuel efficiency.
fFuelEfficiency :: SField FFuelEfficiency
fFuelEfficiency = SField


-- | Data type for emission rate.
type EmissionRate = Double


-- | Field type for emission rate.
type FEmissionRate = '("Emission Rate [g/gge]", EmissionRate)


-- | Field label for emission rate.
fEmissionRate :: SField FEmissionRate
fEmissionRate = SField


-- | Data type for vehicle sales.
type Sales = Double


-- | Field type for vehicle sales.
type FSales = '("Sales [veh]", Sales)


-- | Field label for vehicle sales.
fSales :: SField FSales
fSales = SField


-- | Data type for vehicle stock.
type Stock = Double


-- | Field type for vehicle stock.
type FStock = '("Stock [veh]", Stock)


-- | Field label for vehicle stock.
fStock :: SField FStock
fStock = SField


-- | Determine whether a record has stock.
hasStock :: (FStock ∈ vs)
         => k           -- ^ The key.
         -> FieldRec vs -- ^ The value.
         -> Bool        -- ^ Whether the value has vehicle stock.
hasStock = const $ (\x -> x /= 0 && not (isNaN x)) . (fStock <:)


-- | Data type for distance traveled.
type Travel = Double


-- | Field type for distance traveled.
type FTravel = '("Travel [mi]", Travel)


-- | Field label for distance traveled.
fTravel :: SField FTravel
fTravel = SField


-- | Data type for energy consumed.
type Energy = Double


-- | Field type for energy consumed.
type FEnergy = '("Energy [gge]", Energy)


-- | Field label for energy consumed.
fEnergy :: SField FEnergy
fEnergy = SField


-- | Data type for pollutants emitted.
type Emission = Double


-- | Field type for pollutants emitted.
type FEmission = '("Emission [g]", Emission)


-- | Field label for pollutants emitted.
fEmission :: SField FEmission
fEmission = SField
