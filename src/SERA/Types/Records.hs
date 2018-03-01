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
-- | Types for data records.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds #-}


module SERA.Types.Records (
-- * Data records
  Cash
, Construction
, DemandRec
, Flow
, Geometry
, Impact
, Link
, Node
, ProcessCost
) where


import Data.Vinyl.Derived (FieldRec)
import SERA.Types.Fields (FArea, FCapitalCostStretch, FCostCategory, FFuelConsumption, FDelivery, FInfrastructure, FLength, FLocation, FNameplate, FCapitalCost, FDutyCycle, FFixedCost, FFixedCostStretch, FFlow, FFrom, FImpactCategory, FLifetime, FLoss, FMaterial, FNonFuelConsumption, FPosition, FProduction, FProductive, FQuantity, FRent, FSale, FSalvage, FScaling, FTechnology, FTo, FTransmission, FVariableCost, FVariableCostStretch, FX, FY, FYear)


type Cash = FieldRec '[FInfrastructure, FYear, FCostCategory, FSale]


type Construction = FieldRec '[FInfrastructure, FLocation, FTechnology, FProductive, FYear, FLifetime, FNameplate, FDutyCycle, FLength, FCapitalCost, FFixedCost, FVariableCost]


type DemandRec = FieldRec '[FLocation, FYear, FFuelConsumption, FNonFuelConsumption]


type Flow = FieldRec '[FInfrastructure, FYear, FProduction, FFlow, FLoss, FSale, FSalvage]


type Geometry = FieldRec '[FLocation, FPosition, FX, FY]


type Impact = FieldRec '[FInfrastructure, FYear, FMaterial, FImpactCategory, FQuantity, FSale]


type Node = FieldRec '[FLocation, FX, FY, FArea, FProductive, FSale, FRent]


type Link = FieldRec '[FLocation, FFrom, FTo, FLength, FSale, FRent, FTransmission, FDelivery]


type ProcessCost = '[FProductive, FLifetime, FScaling, FCapitalCost, FCapitalCostStretch, FFixedCost, FFixedCostStretch, FVariableCost, FVariableCostStretch]
