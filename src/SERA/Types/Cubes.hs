-----------------------------------------------------------------------------
--
-- Module      :  $Header$
-- Copyright   :  (c) 2018 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Types for data cubes.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Types.Cubes (
-- * Data cubes
  CashCube
, AnnualTravelCube
, ConstructionCube
, ConsumptionCube
, DemandAreaCube
, DemandCube
, EmissionCube
, EmissionRateCube
, EnergyCube
, ExistingCube
, FlowCube
, FuelEfficiencyCube
, FuelSplitCube
, GeometryCube
, ImpactCube
, IntensityCube
, LinkCube
, MarketShareCube
, ModelYearCube
, NodeCube
, PathwayCube
, PriceCube
, ProcessCostCube
, ProcessInputCube
, ProcessOutputCube
, ProductionCube
, PurchasesCube
, RegionalPurchasesCube
, RegionalStockCube
, SaleCube
, StockCube
, SurvivalCube
, TerritoryCube
, ZoneCube
-- * Functions
, SurvivalFunction
, asSurvivalFunction
) where


import Data.Daft.Vinyl.FieldCube (type (↝), type (*↝))
import Data.Vinyl.TypeLevel (type (++))
import SERA.Types.Fields (FArea, FBillable, FCapacity, FCapitalCost, FConsumption, FConsumptionRate, FConsumptionRateStretch, FCost, FCostCategory, FDelivery, FDutyCycle, FExtended, FFixedCost, FFlow, FFormat, FFraction, FFrom, FFuelConsumption, FGeometry, FImpactCategory, FInfrastructure, FIntensity, FLength, FLifetime, FLocation, FLoss, FMaterial, FNameplate, FNetPrice, FNonFuelConsumption, FPathway, FPosition, FPrice, FProduction, FProductionRate, FProductionRateStretch, FProductive, FQuantity, FRent, FSale, FSales, FSalvage, FStage, FTechnology, FTerritory, FTo, FTransmission, FUpstreamMaterial, FVariableCost, FX, FY, FYear, FYield, FZone, Region, FRegion, fRegion, Age, FAge, fAge, FAnnualTravel, FEmission, FEmissionRate, FEnergy, FFuel, FFuelEfficiency, FFuelSplit, FMarketShare, FModelYear, FPollutant, FPurchases, FStock, Survival, FSurvival, fSurvival, FTravel, FVehicle, Vocation, FVocation, fVocation)
import SERA.Types.Records (ProcessCost)
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Maybe (fromMaybe)

 
type CashCube = '[FInfrastructure, FYear, FCostCategory] *↝ '[FSale, FGeometry]


type ConstructionCube = '[FInfrastructure] *↝ '[FLocation, FTechnology, FProductive, FYear, FLifetime, FNameplate, FDutyCycle, FLength, FCapitalCost, FFixedCost, FVariableCost, FGeometry]


type ConsumptionCube key = (FMaterial ': key) *↝ '[FConsumptionRate, FConsumptionRateStretch]


type DemandAreaCube = '[FLocation, FYear] *↝ '[FFuelConsumption, FNonFuelConsumption, FArea]


type DemandCube = '[FLocation, FYear] *↝ '[FFuelConsumption, FNonFuelConsumption]


type ExistingCube = '[FInfrastructure] *↝ '[FTechnology, FLocation, FYear, FCapacity, FYield, FCost]


type FlowCube = '[FInfrastructure, FYear] *↝ '[FTechnology, FProduction, FFlow, FLoss, FSale, FSalvage, FGeometry]


type GeometryCube = '[FLocation, FPosition] *↝ '[FX, FY]


type ImpactCube = '[FInfrastructure, FYear, FMaterial, FImpactCategory] *↝ '[FQuantity, FSale, FGeometry]


type IntensityCube key = (FMaterial ': FUpstreamMaterial ': FYear ': key) *↝ '[FIntensity]


type LinkCube = '[FLocation] *↝ '[FFrom, FTo, FLength, FSale, FRent, FTransmission, FDelivery]


type NodeCube = '[FLocation] *↝ '[FX, FY, FArea, FProductive, FSale, FRent]


type PathwayCube = '[FPathway, FStage] *↝  '[FTechnology, FYield, FExtended, FTransmission, FDelivery, FFormat]


type PriceCube key = (key ++ '[FMaterial, FYear, FQuantity]) *↝ '[FPrice, FBillable]


type ProcessCostCube = ProcessKey *↝ ProcessCost


type ProcessInputCube = ConsumptionCube ProcessKey'


type ProcessKey = '[FTechnology, FYear, FNameplate, FDutyCycle]


type ProcessKey' = '[FTechnology, FYear, FNameplate]


type ProcessOutputCube = ProductionCube ProcessKey'


type ProductionCube key = (FMaterial ': key) *↝ '[FProductionRate, FProductionRateStretch]


type SaleCube = '[FTerritory, FYear] *↝ '[FProduction, FSale, FCost, FConsumption, FSales, FNetPrice, FGeometry]


type TerritoryCube = '[FTerritory, FLocation] *↝ '[FFraction]


type ZoneCube key = (FZone ': key) *↝ '[FFraction]



type ModelYearCube = '[FModelYear] ↝ '[]


-- | Vehicle sales as a function of region, and model year.
type RegionalPurchasesCube  = '[       FRegion                           , FModelYear                   ] ↝ '[FPurchases         ]


-- | Market share as a function of region, vocation, vehicle type, and model year.
type MarketShareCube    = '[       FRegion, FVocation, FVehicle      , FModelYear                   ] ↝ '[FMarketShare   ]


-- | Fraction of vehicles surviving to a given age, as a function of vocation.
type SurvivalCube       = '[       FRegion, FVocation          , FAge                               ] ↝ '[FSurvival      ]


-- | Annual distance traveled as a function of vocation and age.
type AnnualTravelCube   = '[       FRegion, FVocation          , FAge                               ] ↝ '[FAnnualTravel  ]


-- | Fraction of fuel consumed as a function of vocation and vehicle type.
type FuelSplitCube      = '[                FVocation, FVehicle                  , FFuel            ] ↝ '[FFuelSplit     ]


-- | Fuel efficiency on a given fuel as a function of vehicle type and model year.
type FuelEfficiencyCube = '[                           FVehicle      , FModelYear, FFuel            ] ↝ '[FFuelEfficiency]


-- | Pollutants emitted as a function of vehicle type and model year.
type EmissionRateCube   = '[                           FVehicle      , FModelYear, FFuel, FPollutant] ↝ '[FEmissionRate  ]


-- | Vehicle sales, stock, travel, and energy consumed as a function of calendar year, region, vocation, vehicle type, and model year.
type PurchasesCube          = '[FYear, FRegion, FVocation, FVehicle      , FModelYear                   ] ↝ '[FPurchases, FStock, FTravel, FEnergy          ]


-- | Vehicle sales, stock, travel, and energy consumed as a function of calendar year, region, vocation, and vehicle type.
type StockCube          = '[FYear, FRegion, FVocation, FVehicle                                     ] ↝ '[FPurchases, FStock, FTravel, FEnergy          ]


-- | Energy consumed as a function of calendar year, region, vocation, vehicle type, and fuel.
type EnergyCube         = '[FYear, FRegion, FVocation, FVehicle                  , FFuel            ] ↝ '[                         FEnergy          ]


-- | Polutants emitted as a function of calendar year, region, vocation, vehicle type and fuel.
type EmissionCube       = '[FYear, FRegion, FVocation, FVehicle                  , FFuel, FPollutant] ↝ '[                                 FEmission]


-- | Stock as a function of year, region, vocation, and vehicle type.
type RegionalStockCube  = '[FYear, FRegion, FVocation, FVehicle                                     ] ↝ '[FStock                                    ]


-- | Survival function.
type SurvivalFunction = Region -> Vocation -> Age -> Survival


-- | Convert a survival cube to a survival function.
asSurvivalFunction :: SurvivalCube -> SurvivalFunction
asSurvivalFunction cube region vocation age =
  fromMaybe 0
    $   (fSurvival <:)
    <$> evaluate cube (fRegion =: region <+> fVocation =: vocation <+> fAge =: age)
