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



module SERA.Vehicle.Stock (
-- * Computation
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
import SERA.Vehicle.Stock.Types (AnnualTravelCube, EmissionRateCube, EmissionCube, EnergyCube, FuelEfficiencyCube, FuelSplitCube, MarketShareCube, RegionalSalesCube, RegionalStockCube, SalesCube, StockCube, SurvivalCube, SurvivalFunction, asSurvivalFunction)
import SERA.Vehicle.Types (FAge, fAge, fAnnualTravel, fEmission, fEmissionRate, fEnergy, FFuel, fFuelEfficiency, fFuelSplit, fMarketShare, ModelYear, FModelYear, fModelYear, Sales, FSales, fSales, Stock, FStock, fStock, fSurvival, fTravel, FVehicle, Vocation, FVocation, fVocation)

import qualified Data.Set as S (map)


-- | Add calendar year to a key.
byYear :: '[FRegion, FModelYear, FVocation, FVehicle, FAge] ↝ v  -- ^ The data cube.
       -> '[FYear, FRegion, FVocation, FVehicle, FModelYear] ↝ v -- ^ The data cube year added to the key.
byYear =
  rekey Rekeyer{..}
    where
      rekeyer   rec = τ (rec <+> fYear =: fAge  <: rec + fModelYear <: rec)
      unrekeyer rec = τ (rec <+> fAge  =: fYear <: rec - fModelYear <: rec)


-- | Compute the vehicle stock.
computeStock :: RegionalSalesCube                                 -- ^ Regional sales.
             -> MarketShareCube                                   -- ^ Market share.
             -> SurvivalCube                                      -- ^ Vehicle survival.
             -> AnnualTravelCube                                  -- ^ Annual travel.
             -> FuelSplitCube                                     -- ^ Fuel split.
             -> FuelEfficiencyCube                                -- ^ Fuel efficiency.
             -> EmissionRateCube                                  -- ^ Emission rate.
             -> (SalesCube, StockCube, EnergyCube, EmissionCube)  -- ^ Sales, stock, energy consumed, and pollutants emitted.
computeStock regionalSales marketShares survival annualTravel fuelSplit fuelEfficiency emissionRate = -- FIXME: Review for opportunities to simplify.
  let
    modelYears = ω regionalSales :: Set (FieldRec '[FModelYear])
    years = ((fYear =:) . (fModelYear <:)) `S.map` modelYears
    fuels = ω fuelEfficiency :: Set (FieldRec '[FFuel])
    traveling key rec =
      let
        sales = fSales <: rec * fMarketShare <: rec
        sales' = if fAge <: key == 0 then sales else 0
        stock = sales * fSurvival <: rec
        travel' = stock * fAnnualTravel <: rec
      in
            fSales  =: sales'
        <+> fStock  =: stock
        <+> fTravel =: travel'
    travel =
      ρ (years × ω marketShares)
      $ byYear
      (
        π traveling
        $ regionalSales
        ⋈ marketShares
        ⋈ survival
        ⋈ annualTravel
      )
    consuming _ rec =
      let
        split = fFuelSplit <: rec
        sales = split * fSales <: rec
        stock = split * fStock <: rec
        travel' = split * fTravel <: rec
        energy' = travel' / fFuelEfficiency <: rec
      in
            fSales  =: sales
        <+> fStock  =: stock
        <+> fTravel =: travel'
        <+> fEnergy =: energy'
    energy =
      π consuming
      $ θ travel
      ⋈ fuelSplit
      ⋈ fuelEfficiency
    emitting _ rec =
      fEmission =: fEnergy <: rec * fEmissionRate <: rec
    emission =
      π emitting
      $ energy
      ⋈ emissionRate
    total _ xs =
          fSales  =: sum ((fSales  <:) <$> xs)
      <+> fStock  =: sum ((fStock  <:) <$> xs)
      <+> fTravel =: sum ((fTravel <:) <$> xs)
      <+> fEnergy =: sum ((fEnergy <:) <$> xs)
    totalEmission _ xs =
          fEmission =: sum ((fEmission <:) <$> xs)
  in
    (
      κ               fuels  total           energy
    , κ (modelYears × fuels) total           energy
    , κ  modelYears          ((τ .) . total) energy
    , κ  modelYears          totalEmission   emission
    )


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
inferSales {- FIXME: Implement padding. -} padding survival regionalStock = -- FIXME: Review for opportunities to simplify.
  let
    years = ω regionalStock :: Set (FieldRec '[FYear])
    modelYears = ((fModelYear =:) . (fYear <:)) `S.map` (ω regionalStock :: Set (FieldRec '[FYear]))
    vocationsVehicles = ω regionalStock :: Set (FieldRec '[FVocation, FVehicle])
    regionalStock' =
      π prependYear regionalStock
        where
          prependYear :: FieldRec '[FYear, FRegion, FVocation, FVehicle] -> FieldRec '[FStock] -> FieldRec '[FYear, FStock]
          prependYear = (<+>) . (fYear =:) . (fYear <:)
    salesLists =
      κ years invertSales regionalStock'
        where
          invertSales :: FieldRec '[FRegion, FVocation, FVehicle] -> [FieldRec '[FYear, FStock]] -> FieldRec '[FSalesList]
          invertSales key recs =
            fSalesList =:
              inverseSurvivalFunction
                (asSurvivalFunction survival)
                (fVocation <: key)
                (((fYear <:) &&& (fStock <:)) <$> recs)
    sales =
      δ modelYears disaggregateSales salesLists
        where
          disaggregateSales :: FieldRec '[FRegion, FVocation, FVehicle, FModelYear] -> FieldRec '[FSalesList] -> FieldRec '[FSales]
          disaggregateSales = (((fSales =:) . fromMaybe 0) . ) . (. (fSalesList <:)) . lookup . (fModelYear <:)
    regionalSales =
      κ vocationsVehicles sumSales sales
        where
          sumSales = const $ (fSales =:) . sum . map (fSales <:)
    marketShare =
      π fractionalizeSales
      $ sales
      ⋈ π relabelSales regionalSales
      where
        fractionalizeSales _ rec = fMarketShare =: fSales <: rec / fTotalSales <: rec
        relabelSales = const $ (fTotalSales =:) . (fSales <:)
  in
    (
      regionalSales
    , marketShare
    )


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
