-----------------------------------------------------------------------------
--
-- Module      :  SERA.Vehicle.Stock
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Vehicle stock modeling.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeOperators             #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module SERA.Vehicle.Stock (
-- * Computations
  computeStock
, inferSales
) where


import Control.Arrow ((&&&))
import Data.Daft.DataCube
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec, SField(..))
import SERA.Types (FRegion, Year, FYear, fYear)
import SERA.Vehicle.Stock.Types (AnnualTravelCube, EmissionRateCube, EmissionCube, EnergyCube, FuelEfficiencyCube, FuelSplitCube, MarketShareCube, RegionalSalesCube, RegionalStockCube, SalesCube, StockCube, SurvivalCube)
import SERA.Vehicle.Types (Age, FAge, fAge, FAnnualTravel, fAnnualTravel, FEmission, fEmission, FEmissionRate, fEmissionRate, FEnergy, fEnergy, FFuel, FFuelEfficiency, fFuelEfficiency, FFuelSplit, fFuelSplit, FMarketShare, fMarketShare, ModelYear, FModelYear, fModelYear, FPollutant, Sales, FSales, fSales, Stock, FStock, fStock, Survival, FSurvival, fSurvival, FTravel, fTravel, FVehicle, Vocation, FVocation, fVocation)

import qualified Data.Set as S (map)


-- | Add calendar year to a key.
byYear :: '[FRegion, FModelYear, FVocation, FVehicle, FAge] ↝ v -> '[FYear, FRegion, FVocation, FVehicle, FModelYear] ↝ v
byYear =
  rekey
    $ Rekeyer{..}
    where
      rekeyer   rec = τ (rec <+> fYear =: fAge  <: rec + fModelYear <: rec)
      unrekeyer rec = τ (rec <+> fAge  =: fYear <: rec - fModelYear <: rec)


-- FIXME: Throughout this module, use lens arithmetic to avoid all of the getting and setting.  The basic pattern can be 'τ $ . . . lens arithmetic . . . $ mconcat [ . . . records providing field . . . ]'.


-- | Compute sales, stock, and distance traveled.
traveling :: FieldRec '[FRegion, FModelYear, FVocation, FVehicle, FAge] -> FieldRec '[FSales, FMarketShare, FSurvival, FAnnualTravel] -> FieldRec '[FSales, FStock, FTravel]
traveling key rec =
  let
    sales = fSales <: rec * fMarketShare <: rec
    sales' = if fAge <: key == 0 then sales else 0
    stock = sales * fSurvival <: rec
    travel = stock * fAnnualTravel <: rec
  in
        fSales  =: sales'
    <+> fStock  =: stock
    <+> fTravel =: travel


-- | Compute energy consumption.
consuming :: FieldRec '[FYear, FRegion, FVocation, FVehicle, FModelYear, FFuel] -> FieldRec '[FSales, FStock, FTravel, FFuelSplit, FFuelEfficiency] -> FieldRec '[FSales, FStock, FTravel, FEnergy]
consuming _ rec =
  let
    split = fFuelSplit <: rec
    sales = split * fSales <: rec
    stock = split * fStock <: rec
    travel = split * fTravel <: rec
    energy = travel / fFuelEfficiency <: rec
  in
        fSales  =: sales
    <+> fStock  =: stock
    <+> fTravel =: travel
    <+> fEnergy =: energy


-- | Compute the emission of pollutants.
emitting :: FieldRec '[FYear, FRegion, FVocation, FVehicle, FModelYear, FFuel, FPollutant] -> FieldRec '[FSales, FStock, FTravel, FEnergy, FEmissionRate] -> FieldRec '[FEmission]
emitting _ rec = fEmission =: fEnergy <: rec * fEmissionRate <: rec


-- | Total sales, stock, travel, and energy.
total :: k -> [FieldRec '[FSales, FStock, FTravel, FEnergy]] -> FieldRec '[FSales, FStock, FTravel, FEnergy]
total _ xs =
      fSales  =: sum ((fSales  <:) <$> xs)
  <+> fStock  =: sum ((fStock  <:) <$> xs)
  <+> fTravel =: sum ((fTravel <:) <$> xs)
  <+> fEnergy =: sum ((fEnergy <:) <$> xs)


-- | Total emissions.
totalEmission :: k -> [FieldRec '[FEmission]] -> FieldRec '[FEmission]
totalEmission _ xs =
      fEmission =: sum ((fEmission <:) <$> xs)


-- | Compute the vehicle stock.
computeStock :: RegionalSalesCube                                 -- ^ Regional sales.
             -> MarketShareCube                                   -- ^ Market share.
             -> SurvivalCube                                      -- ^ Vehicle survival.
             -> AnnualTravelCube                                  -- ^ Annual travel.
             -> FuelSplitCube                                     -- ^ Fuel split.
             -> FuelEfficiencyCube                                -- ^ Fuel efficiency.
             -> EmissionRateCube                                  -- ^ Emission rate.
             -> (SalesCube, StockCube, EnergyCube, EmissionCube)  -- ^ Sales, stock, energy consumed, and pollutants emitted.
computeStock regionalSales marketShares survival annualTravel fuelSplit fuelEfficiency emissionRate =
  let
    modelYears = ω regionalSales :: Set (FieldRec '[FModelYear])
    years = ((fYear =:) . (fModelYear <:)) `S.map` modelYears
    fuels = ω fuelEfficiency :: Set (FieldRec '[FFuel])
    pollutants = ω emissionRate :: Set (FieldRec '[FPollutant])
    support = years × ω marketShares
    support' = support × fuels
    support'' = support' × pollutants
    travel =
      ρ support -- Reifying here requires a little more memory, but saves some time.
      $ byYear
      (
        π traveling
        $ regionalSales
        ⋈ marketShares
        ⋈ survival
        ⋈ annualTravel
      )
    energy =
      π consuming
        $ travel
        ⋈ fuelSplit
        ⋈ fuelEfficiency
    emission =
      π emitting
      $ energy
      ⋈ emissionRate
  in
    (
      κ support'           total  energy
    , κ support'           total  energy
    , κ support'  ((τ .) . total) energy
    , κ support'' totalEmission   emission
    )


-- | Field type for stock by year.
type FStockList = '("Stock", [(Year, Stock)])


-- | Field label for stock by year.
fStockList :: SField FStockList
fStockList = SField


-- | Field type for sales by model year.
type FSalesList = '("Sales", [(ModelYear, Sales)])


-- | Field label for sales by model year.
fSalesList :: SField FSalesList
fSalesList = SField


-- | Field type for total sales.
type FTotalSales = '("Total Sales", Sales)


-- | Field label for total sales.
fTotalSales :: SField FTotalSales
fTotalSales = SField


-- | Infer vehicle sales from vehicle stock.
inferSales :: Int                                  -- ^ Number of prior years to generate sales for the stock.
           -> SurvivalCube                         -- ^ Vehicle survival.
           -> RegionalStockCube                    -- ^ Regional vehicle stock.
           -> (RegionalSalesCube, MarketShareCube) -- ^ Regional vehicle sales and market share.
inferSales padding survival regionalStock = --FIXME: Implement padding.
  let
    modelYears = ((fModelYear =:) . (fYear <:)) `S.map` (ω regionalStock :: Set (FieldRec '[FYear]))
    support = ω regionalStock :: Set (FieldRec '[FYear, FRegion, FVocation, FVehicle])
    support' = support × modelYears
    support'' = τ `S.map` support' :: Set (FieldRec '[FRegion, FModelYear])
    regionalStock' =
      π prependYear regionalStock
        where
          prependYear :: FieldRec '[FYear, FRegion, FVocation, FVehicle] -> FieldRec '[FStock] -> FieldRec '[FYear, FStock]
          prependYear = (<+>) . (fYear =:) . (fYear <:)
    salesLists =
      κ support invertSales regionalStock'
        where
          invertSales :: FieldRec '[FRegion, FVocation, FVehicle] -> [FieldRec '[FYear, FStock]] -> FieldRec '[FSalesList]
          invertSales key recs =
            fSalesList =:
              inverseSurvivalFunction
                (asSurvivalFunction survival)
                (fVocation <: key)
                (((fYear <:) &&& (fStock <:)) <$> recs)
    sales =
      δ support' disaggregateSales salesLists
        where
          disaggregateSales :: FieldRec '[FRegion, FVocation, FVehicle, FModelYear] -> FieldRec '[FSalesList] -> FieldRec '[FSales]
          disaggregateSales = (((fSales =:) . fromMaybe 0) . ) . (. (fSalesList <:)) . lookup . (fModelYear <:)
    regionalSales =
      κ support' sumSales sales
        where
          sumSales = const $ (fSales =:) . sum . map (fSales <:)
    fractionalizeSales _ rec = fMarketShare =: fSales <: rec / fTotalSales <: rec
    regionalSales' = π (const ((fTotalSales =:) . (fSales <:))) regionalSales
  in
    (
      regionalSales
    , π fractionalizeSales $ sales ⋈ regionalSales'
    )


-- | Survival function.
type SurvivalFunction = Vocation -> Age -> Survival


-- | Convert a survival cube to a survival function.
asSurvivalFunction :: SurvivalCube -> SurvivalFunction
asSurvivalFunction cube vocation age =
  fromMaybe 0
    $   (fSurvival <:)
    <$> evaluate cube (fVocation =: vocation <+> fAge =: age)


-- | Invert a survival function, using back substitution.
inverseSurvivalFunction :: SurvivalFunction     -- ^ The survival function.
                        -> Vocation             -- ^ The vehicles being classified.
                        -> [(Year, Stock)]      -- ^ The vehicle stock to be inverted.
                        -> [(ModelYear, Sales)] -- ^ The vehicle sales.
inverseSurvivalFunction survival vocation stocks =
  let
    (year0, year1) = (minimum &&& maximum) $ fst <$> stocks
    years = [year0..year1]
    stocks' = map (\y -> fromMaybe (read "NaN") (y `lookup` stocks)) years
    dot = (sum .) . zipWith (*)
    s0 : ss = map (survival vocation) [0..]
    invert sales []                = sales
    invert sales (stock : stockss) = invert ((stock - ss `dot` sales) / s0 : sales) stockss
    invert _     _                 = undefined
  in
    zip years
      . reverse
      $ invert [] stocks'
