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
, inferMarketShares
) where


import Data.Daft.DataCube
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (rcast)
import SERA.Types (FRegion, FYear, fYear)
import SERA.Vehicle.Stock.Types (AnnualTravelCube, EmissionRateCube, EmissionCube, EnergyCube, FuelEfficiencyCube, FuelSplitCube, MarketShareCube, RegionalSalesCube, SalesCube, StockCube, SurvivalCube)
import SERA.Vehicle.Types (FAge, fAge, FAnnualTravel, fAnnualTravel, FEmission, fEmission, FEmissionRate, fEmissionRate, FEnergy, fEnergy, FFuel, FFuelEfficiency, fFuelEfficiency, FFuelSplit, fFuelSplit, FMarketShare, fMarketShare, FModelYear, fModelYear, FPollutant, FSales, fSales, FStock, fStock, FSurvival, fSurvival, FTravel, fTravel, FVehicle, FVocation)

import qualified Data.Set as S (map)


byYear :: '[FRegion, FModelYear, FVocation, FVehicle, FAge] ↝ v -> '[FYear, FRegion, FVocation, FVehicle, FModelYear] ↝ v
byYear =
  rekey
    $ Rekeyer{..}
    where
      rekeyer   rec = rcast (rec <+> fYear =: fAge  <: rec + fModelYear <: rec)
      unrekeyer rec = rcast (rec <+> fAge  =: fYear <: rec - fModelYear <: rec)


-- FIXME: Throughout this module, use lens arithmetic to avoid all of the getting and setting.  The basic pattern can be 'rcast $ . . . lens arithmetic . . . $ mconcat [ . . . records providing field . . . ]'.


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


emitting :: FieldRec '[FYear, FRegion, FVocation, FVehicle, FModelYear, FFuel, FPollutant] -> FieldRec '[FSales, FStock, FTravel, FEnergy, FEmissionRate] -> FieldRec '[FEmission]
emitting _ rec = fEmission =: fEnergy <: rec * fEmissionRate <: rec


total :: k -> [FieldRec '[FSales, FStock, FTravel, FEnergy]] -> FieldRec '[FSales, FStock, FTravel, FEnergy]
total _ xs =
      fSales  =: sum ((fSales  <:) <$> xs)
  <+> fStock  =: sum ((fStock  <:) <$> xs)
  <+> fTravel =: sum ((fTravel <:) <$> xs)
  <+> fEnergy =: sum ((fEnergy <:) <$> xs)


totalEmission :: k -> [FieldRec '[FEmission]] -> FieldRec '[FEmission]
totalEmission _ xs =
      fEmission =: sum ((fEmission <:) <$> xs)


computeStock :: RegionalSalesCube
             -> MarketShareCube
             -> SurvivalCube
             -> AnnualTravelCube
             -> FuelSplitCube
             -> FuelEfficiencyCube
             -> EmissionRateCube
             -> (SalesCube, StockCube, EnergyCube, EmissionCube)
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
      ρ support
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
      κ support'               total  energy
    , κ support'               total  energy
    , κ support'  ((rcast .) . total) energy
    , κ support'' totalEmission       emission
    )


inferMarketShares = undefined


inferSales = undefined


{-
-- | Infer vehicle sales from vehicle stock.
inferSales :: Int                -- ^ Number of prior years to generate sales for the stock.
           -> SurvivalFunction   -- ^ The vehicle survival function.
           -> [StockRecord]      -- ^ The vehicle stock records.
           -> [SalesStockRecord] -- ^ The vehicle sales and stock records.
inferSales _padding _survival =
  undefined
{-
  let
    inferForClassification :: Classification -> [StockRecord] -> [SalesStockRecord]
    inferForClassification classification classifiedStock =
      let
        (year0, year1) = aggregate rpar (fYear <:) (minimum &&& maximum) classifiedStock
        years = [year0 - padding .. year1]
        inverseSurvival = inverseSurvivalFunction survival classification years
        inferForRegion :: FieldRec '[FRegion, FClassification] -> [StockRecord] -> [SalesStockRecord]
        inferForRegion regionClassification regionalStock =
          let
            stock =
              replicateHead padding
                $ groupExtract rpar (fYear <:) (fStock <:) regionalStock -- FIXME: Check for missing values.
            sales = inverseSurvival stock
          in
            zipWith3
              (\year' sales' stock' -> regionClassification <+> fYear =: year' <+> fSales =: sales' <+> fStock =: stock')
              years sales stock
      in
        groupReduceFlattenByKey rpar rcast inferForRegion classifiedStock
  in
    groupReduceFlattenByKey rpar (fClassification <:) inferForClassification
-}


-- | Infer regional total sales and market shares from vehicle sales.
inferMarketShares :: [SalesStockRecord]                          -- ^ The vehicle sales and stock records.
                  -> ([NewVehiclesRecord], [MarketSharesRecord]) -- ^ The regional total sales and market shares.
inferMarketShares =
  undefined
{-
  let
    inferForRegionYear :: FieldRec '[FRegion, FYear] -> [SalesStockRecord] -> (NewVehiclesRecord, [MarketSharesRecord])
    inferForRegionYear regionYear salesStocks =
      let
        model = fModelYear =: (fYear <: regionYear)
        sales = aggregate rpar (fSales <:) sum salesStocks
      in
        (
          rcast $ regionYear <+> model <+> fSales =: sales
        , [
            rcast $ x <+> model <+> fMarketShare =: (fSales <: x / sales)
          |
            x <- salesStocks
          ]
        )
  in
    (map fst &&& concatMap snd)
      . groupReduceByKey rpar rcast inferForRegionYear
-}


-- | Invert a survival function, using back substitution.
inverseSurvivalFunction :: SurvivalFunction -- ^ The survival function.
                        -> Classification   -- ^ The vehicles being classified.
                        -> [Year]           -- ^ The years for which to invert.
                        -> [Stock]          -- ^ The vehicle stock to be inverted.
                        -> [Sales]          -- ^ The vehicle sales.
inverseSurvivalFunction survival classification _ =
  let
    dot = (sum .) . zipWith (*)
    s0 : ss = map (survival classification) [0..]
    invert sales []               = sales
    invert sales (stock : stocks) = invert ((stock - ss `dot` sales) / s0 : sales) stocks
  in
    reverse . invert []

-}
