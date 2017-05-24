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
{-# LANGUAGE TemplateHaskell            #-}
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


import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import SERA.Types.TH (makeField, makeStringField)


-- | Data type for vehicle vocation.
-- | Field type for vehicle vocation.
-- | Field label for vehicle vocation.
$(makeStringField "Vocation" "Vocation")


-- | Data type for vehicle type.
-- | Field type for vehicle type.
-- | Field label for vehicle type.
$(makeStringField "Vehicle" "Vehicle")


-- | Data type for vehicle age.
type Age = Int


-- | Field type for vehicle age.
-- | Field label for vehicle age.
$(makeField "Age" "Age [yr]" ''Age)


-- | Data type for model year.
type ModelYear = Int


-- | Field type for model year.
$(makeField "ModelYear" "Model Year" ''ModelYear)


-- | Field label for model year.
-- | Data type for fuel.
-- | Field type for fuel.
-- | Field label for fuel.
$(makeStringField "Fuel" "Fuel")


-- | Data type for pollutant type.
-- | Field type for pollutant type.
-- | Field label for pollutant type.
$(makeStringField "Pollutant" "Pollutant")


-- | Data type for market share.
type MarketShare = Double


-- | Field type for market share.
-- | Field label for market share.
$(makeField "MarketShare" "Market Share [veh/veh]" ''MarketShare)


-- | Data type for relative market share.
type RelativeMarketShare = Double


-- | Field type for relative market share.
-- | Field label for relative market share.
$(makeField "RelativeMarketShare" "Relative Market Share" ''RelativeMarketShare)


-- | Data type for vehicle survival.
type Survival = Double


-- | Field type for vehicle survival.
-- | Field label for vehicle survival.
$(makeField "Survival" "Surviving Vehicles [veh/veh]" ''Survival)


-- | Data type for annual travel.
type AnnualTravel = Double


-- | Field type for annual travel.
-- | Field label for annual travel.
$(makeField "AnnualTravel" "Annual Travel [mi/yr]" ''AnnualTravel)


-- | Data type for fuel split.
type FuelSplit = Double


-- | Field type for fuel split.
-- | Field label for fuel split.
$(makeField "FuelSplit" "Fraction of Travel [mi/mi]" ''FuelSplit)


-- | Data type for fuel efficiency.
type FuelEfficiency = Double


-- | Field type for fuel efficiency.
-- | Field label for fuel efficiency.
$(makeField "FuelEfficiency" "Fuel Efficiency [mi/gge]" ''FuelEfficiency)


-- | Data type for emission rate.
type EmissionRate = Double


-- | Field type for emission rate.
-- | Field label for emission rate.
$(makeField "EmissionRate" "Emission Rate [g/gge]" ''EmissionRate)


-- | Data type for vehicle sales.
type Sales = Double


-- | Field type for vehicle sales.
-- | Field label for vehicle sales.
$(makeField "Sales" "Sales [veh]" ''Sales)


-- | Data type for vehicle stock.
type Stock = Double


-- | Field type for vehicle stock.
-- | Field label for vehicle stock.
$(makeField "Stock" "Stock [veh]" ''Stock)


-- | Determine whether a record has stock.
hasStock :: (FStock ∈ vs)
         => k           -- ^ The key.
         -> FieldRec vs -- ^ The value.
         -> Bool        -- ^ Whether the value has vehicle stock.
hasStock = const $ (\x -> x /= 0 && not (isNaN x)) . (fStock <:)


-- | Data type for distance traveled.
type Travel = Double


-- | Field type for distance traveled.
-- | Field label for distance traveled.
$(makeField "Travel" "Travel [mi]" ''Travel)


-- | Data type for energy consumed.
type Energy = Double


-- | Field type for energy consumed.
-- | Field label for energy consumed.
$(makeField "Energy" "Energy [gge]" ''Energy)


-- | Data type for pollutants emitted.
type Emission = Double


-- | Field type for pollutants emitted.
-- | Field label for pollutants emitted.
$(makeField "Emission" "Emission [g]" ''Emission)
