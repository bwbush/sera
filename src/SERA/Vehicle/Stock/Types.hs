-----------------------------------------------------------------------------
--
-- Module      :  SERA.Vehicle.Stock.Types
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Types for vehicle stock modeling.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE Trustworthy   #-}
{-# LANGUAGE TypeOperators #-}


module SERA.Vehicle.Stock.Types (
-- * Inputs
  RegionalSalesCube
, MarketShareCube
, SurvivalCube
, AnnualTravelCube
, FuelSplitCube
, FuelEfficiencyCube
, EmissionRateCube
, RegionalStockCube
-- * Outputs
, SalesCube
, StockCube
, EnergyCube
, EmissionCube
) where

import Data.Daft.Vinyl.FieldCube (type (↝))
import SERA.Types (FRegion, FYear)
import SERA.Vehicle.Types (FAge, FAnnualTravel, FEmission, FEmissionRate, FEnergy, FFuel, FFuelEfficiency, FFuelSplit, FMarketShare, FModelYear, FPollutant, FSales, FStock, FSurvival, FTravel, FVehicle, FVocation)


-- | Vehicle sales as a function of region, and model year,.
type RegionalSalesCube  = '[       FRegion                           , FModelYear                   ] ↝ '[FSales         ]


-- | Market share as a function of region, vocation, vehicle type, and model year.
type MarketShareCube    = '[       FRegion, FVocation, FVehicle      , FModelYear                   ] ↝ '[FMarketShare   ]


-- | Fraction of vehicles surviving to a given age, as a function of vocation.
type SurvivalCube       = '[                FVocation          , FAge                               ] ↝ '[FSurvival      ]


-- | Annual distance traveled as a function of vocation and age.
type AnnualTravelCube   = '[                FVocation          , FAge                               ] ↝ '[FAnnualTravel  ]


-- | Fraction of fuel consumed as a function of vocation and vehicle type.
type FuelSplitCube      = '[                FVocation, FVehicle                  , FFuel            ] ↝ '[FFuelSplit     ]


-- | Fuel efficiency on a given fuel as a function of vehicle type and model year.
type FuelEfficiencyCube = '[                           FVehicle      , FModelYear, FFuel            ] ↝ '[FFuelEfficiency]


-- | Pollutants emitted as a function of vehicle type and model year.
type EmissionRateCube   = '[                           FVehicle      , FModelYear, FFuel, FPollutant] ↝ '[FEmissionRate  ]


-- | Vehicle sales, stock, travel, and energy consumed as a function of calendar year, region, vocation, vehicle type, and model year.
type SalesCube          = '[FYear, FRegion, FVocation, FVehicle      , FModelYear                   ] ↝ '[FSales, FStock, FTravel, FEnergy          ]


-- | Vehicle sales, stock, travel, and energy consumed as a function of calendar year, region, vocation, and vehicle type.
type StockCube          = '[FYear, FRegion, FVocation, FVehicle                                     ] ↝ '[FSales, FStock, FTravel, FEnergy          ]


-- | Energy consumed as a function of calendar year, region, vocation, vehicle type, and fuel.
type EnergyCube         = '[FYear, FRegion, FVocation, FVehicle                  , FFuel            ] ↝ '[                         FEnergy          ]


-- | Polutants emitted as a function of calendar year, region, vocation, vehicle type and fuel.
type EmissionCube       = '[FYear, FRegion, FVocation, FVehicle                  , FFuel, FPollutant] ↝ '[                                 FEmission]


-- | Stock as a function of year, region, vocation, and vehicle type.
type RegionalStockCube  = '[FYear, FRegion, FVocation, FVehicle                                     ] ↝ '[FStock                                    ]
