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
, ConstructionCube
, ConsumptionCube
, DemandAreaCube
, DemandCube
, ExistingCube
, FlowCube
, GeometryCube
, ImpactCube
, IntensityCube
, LinkCube
, NodeCube
, PathwayCube
, PriceCube
, ProcessCostCube
, ProcessInputCube
, ProcessOutputCube
, ProductionCube
, SaleCube
, TerritoryCube
, ZoneCube
) where


import Data.Daft.Vinyl.FieldCube (type (*↝))
import SERA.Types.Fields (FArea, FBillable, FCapacity, FCapitalCost, FConsumption, FConsumptionRate, FConsumptionRateStretch, FCost, FCostCategory, FDelivery, FDutyCycle, FExtended, FFixedCost, FFlow, FFormat, FFraction, FFrom, FFuelConsumption, FGeometry, FImpactCategory, FInfrastructure, FIntensity, FLength, FLifetime, FLocation, FLoss, FMaterial, FNameplate, FNetPrice, FNonFuelConsumption, FPathway, FPosition, FPrice, FProduction, FProductionRate, FProductionRateStretch, FProductive, FQuantity, FRent, FSale, FSales, FSalvage, FStage, FTechnology, FTerritory, FTo, FTransmission, FUpstreamMaterial, FVariableCost, FX, FY, FYear, FYield, FZone)
import SERA.Types.Records (ProcessCost)

 
type CashCube = '[FInfrastructure, FYear, FCostCategory] *↝ '[FSale, FGeometry]


type ConstructionCube = '[FInfrastructure] *↝ '[FLocation, FTechnology, FProductive, FYear, FLifetime, FNameplate, FDutyCycle, FLength, FCapitalCost, FFixedCost, FVariableCost, FGeometry]


type ConsumptionCube key = (FMaterial ': key) *↝ '[FConsumptionRate, FConsumptionRateStretch]


type DemandAreaCube = '[FLocation, FYear] *↝ '[FFuelConsumption, FNonFuelConsumption, FArea]


type DemandCube = '[FLocation, FYear] *↝ '[FFuelConsumption, FNonFuelConsumption]


type ExistingCube = '[FInfrastructure] *↝ '[FLocation, FYear, FCapacity, FYield, FCost]


type FlowCube = '[FInfrastructure, FYear] *↝ '[FTechnology, FProduction, FFlow, FLoss, FSale, FSalvage, FGeometry]


type GeometryCube = '[FLocation, FPosition] *↝ '[FX, FY]


type ImpactCube = '[FInfrastructure, FYear, FMaterial, FImpactCategory] *↝ '[FQuantity, FSale, FGeometry]


type IntensityCube key = (FMaterial ': FUpstreamMaterial ': FYear ': key) *↝ '[FIntensity]


type LinkCube = '[FLocation] *↝ '[FFrom, FTo, FLength, FSale, FRent, FTransmission, FDelivery]


type NodeCube = '[FLocation] *↝ '[FX, FY, FArea, FProductive, FSale, FRent]


type PathwayCube = '[FPathway, FStage] *↝  '[FTechnology, FYield, FExtended, FTransmission, FDelivery, FFormat]


type PriceCube key = (FMaterial ': FYear ': key) *↝ '[FPrice, FBillable]


type ProcessCostCube = ProcessKey *↝ ProcessCost


type ProcessInputCube = ConsumptionCube ProcessKey'


type ProcessKey = '[FTechnology, FYear, FNameplate, FDutyCycle]


type ProcessKey' = '[FTechnology, FYear, FNameplate]


type ProcessOutputCube = ProductionCube ProcessKey'


type ProductionCube key = (FMaterial ': key) *↝ '[FProductionRate, FProductionRateStretch]


type SaleCube = '[FTerritory, FYear] *↝ '[FProduction, FSale, FCost, FConsumption, FSales, FNetPrice]


type TerritoryCube = '[FTerritory, FLocation] *↝ '[FFraction]


type ZoneCube key = (FZone ': key) *↝ '[FFraction]
