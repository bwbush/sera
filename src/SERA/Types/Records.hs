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
) where


import Data.Vinyl.Derived (FieldRec)
import SERA.Material.Types (FMaterial)
import SERA.Network.Types (FLength, FInfrastructure, FLocation, FSale, FX, FY)
import SERA.Process.Types (FNameplate, FCapitalCost, FDutyCycle, FFixedCost, FLifetime, FProductive, FTechnology, FVariableCost)
import SERA.Types (FYear)
import SERA.Types.Fields -- FIXME: Import explicityly.


type Cash = FieldRec '[FInfrastructure, FYear, FCostCategory, FSale]


type Construction = FieldRec '[FInfrastructure, FLocation, FTechnology, FProductive, FYear, FLifetime, FNameplate, FDutyCycle, FLength, FCapitalCost, FFixedCost, FVariableCost]


type DemandRec = FieldRec '[FLocation, FYear, FFuelConsumption, FNonFuelConsumption]


type Flow = FieldRec '[FInfrastructure, FYear, FProduction, FFlow, FLoss, FSale, FSalvage]


type Geometry = FieldRec '[FLocation, FPosition, FX, FY]


type Impact = FieldRec '[FInfrastructure, FYear, FMaterial, FImpactCategory, FQuantity, FSale]
