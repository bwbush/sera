{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE Trustworthy   #-}


module SERA.Vehicle.Stock.Types (
  NewVehiclesFunction
, NewVehiclesRecord
, MarketSharesFunction
, MarketSharesRecord
, FuelSplitsFunction
, FuelSplitsRecord
, SurvivalFunction
, SurvivalRecord
, AnnualTravelFunction
, AnnualTravelRecord
, FuelEconomyFunction
, FuelEconomyRecord
, StockRecord
, SalesStockRecord
) where


import Data.Vinyl.Derived (FieldRec)
import SERA.Types (FRegion, FYear, Region)
import SERA.Vehicle.Types (Age, AnnualTravel, Classification, FAge, FAnnualTravel, FFuelSplit, FClassification, FFuel, FFuelEconomy, FMarketShare, FModelYear, FSales, FStock, FSurvival, Fuel, FuelEconomy, FuelSplit, MarketShare, ModelYear, Sales, Survival)


type NewVehiclesFunction = Region -> ModelYear -> Sales


type NewVehiclesRecord = FieldRec '[FRegion, FModelYear, FSales]


type MarketSharesFunction = Region -> Classification -> ModelYear -> MarketShare


type MarketSharesRecord = FieldRec '[FRegion, FClassification, FModelYear, FMarketShare]


type FuelSplitsFunction = Region -> Classification -> ModelYear -> Fuel -> FuelSplit


type FuelSplitsRecord = FieldRec '[FRegion, FClassification, FModelYear, FFuel, FFuelSplit]


type SurvivalFunction = Classification -> Age -> Survival


type SurvivalRecord = FieldRec '[FClassification, FAge, FSurvival]


type AnnualTravelFunction = Region -> Classification -> Age -> Fuel -> AnnualTravel


type AnnualTravelRecord = FieldRec '[FRegion, FClassification, FAge, FFuel, FAnnualTravel]


type FuelEconomyFunction = Region -> Classification -> ModelYear -> Fuel -> FuelEconomy


type FuelEconomyRecord = FieldRec '[FRegion, FClassification, FModelYear, FFuel, FFuelEconomy]


type StockRecord = FieldRec '[FRegion, FClassification, FYear, FStock]


type SalesStockRecord = FieldRec '[FRegion, FClassification, FYear, FSales, FStock]
