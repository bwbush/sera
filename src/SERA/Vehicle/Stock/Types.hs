{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE Trustworthy   #-}
{-# LANGUAGE TypeOperators #-}


module SERA.Vehicle.Stock.Types (
  SurvivalCube
, RegionalSalesCube
, MarketSharesCube
) where

import Data.Daft.Vinyl.FieldCube (type (↝))
import Data.Vinyl.Derived (FieldRec)
import SERA.Types (FRegion, FYear, Region)
import SERA.Vehicle.Types (Age, AnnualTravel, Classification, Emission, EmissionFactor, Fuel, FuelEfficiency, FuelSplit, MarketShare, ModelYear, Sales, Survival, FAge, FAnnualTravel, FEmission, FEmissionFactor, FFuelSplit, FClassification, FFuel, FFuelEfficiency, FMarketShare, FModelYear, FSales, FStock, FSurvival)


type SurvivalCube      = '[FClassification, FModelYear, FYear]          ↝ '[FSurvival]

type RegionalSalesCube = '[FRegion,                         FModelYear] ↝ '[FSales]

type MarketSharesCube  = '[FRegion,        FClassification, FModelYear] ↝ '[FMarketShare]

type SalesCube         = '[FRegion,        FModelYear, FClassification] ↝ '[FSales]

type StockCube         = '[FRegion, FYear, FClassification, FModelYear] ↝ '[FSales, FStock]

type StockCube'        = '[FRegion, FYear, FClassification, FModelYear] ↝ '[FSales, FSurvival]

type SalesStocksCube   = '[FRegion, FClassification, FYear]             ↝ '[FSales, FStock]


type AnnualTravelCube   = '[FRegion, FClassification, FAge] ↝ '[FAnnualTravel]

type EmissionFactorCube = '[FClassification, FModelYear, FEmission] ↝ '[FEmissionRate]

type FuelEfficiencyCube = '[FClassification, FModelYear] ↝ '[FFuelEfficiency]

type FuelSplitCube      = '[FRegion, FYear, FClassification, FFuel] ↝ '[FFractionTravel]
