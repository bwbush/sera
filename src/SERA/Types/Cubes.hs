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
, DemandCube
, FlowCube
, GeometryCube
, ImpactCube
, SaleCube
) where


import Data.Daft.Vinyl.FieldCube (type (*↝))
import SERA.Material.Types (FMaterial)
import SERA.Network.Types (FLength, FInfrastructure, FLocation, FSale, FTerritory, FX, FY)
import SERA.Process.Types (FNameplate, FCapitalCost, FCost, FDutyCycle, FFixedCost, FLifetime, FProductive, FTechnology, FVariableCost)
import SERA.Types (FYear)
import SERA.Types.Fields -- FIXME: Import explicityly.

 
type CashCube = '[FInfrastructure, FYear, FCostCategory] *↝ '[FSale]


type ConstructionCube = '[FInfrastructure] *↝ '[FLocation, FTechnology, FProductive, FYear, FLifetime, FNameplate, FDutyCycle, FLength, FCapitalCost, FFixedCost, FVariableCost]


type DemandCube = '[FLocation, FYear] *↝ '[FFuelConsumption, FNonFuelConsumption]


type FlowCube = '[FInfrastructure, FYear] *↝ '[FProduction, FFlow, FLoss, FSale, FSalvage]


type GeometryCube = '[FLocation, FPosition] *↝ '[FX, FY]


type ImpactCube = '[FInfrastructure, FYear, FMaterial, FImpactCategory] *↝ '[FQuantity, FSale]


type SaleCube = '[FTerritory, FYear] *↝ '[FProduction, FSale, FCost, FConsumption, FSales, FNetPrice]
