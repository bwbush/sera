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
{-# LANGUAGE TypeOperators             #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module SERA.Vehicle.Stock (
  RegionalSalesCube
, MarketSharesCube
, universe
, computeStock
, inferSales
, inferMarketShares
) where


import Data.Daft.DataCube
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec, (=:))
import Data.Vinyl.Lens (rcast)
import SERA.Types (FRegion, FYear, Year, fYear)
import SERA.Vehicle.Stock.Types (MarketSharesRecord, NewVehiclesRecord, SalesStockRecord, StockRecord, SurvivalFunction)
import SERA.Vehicle.Types (Classification, FClassification, FMarketShare, FModelYear, FSales, FStock, FSurvival, Sales, Stock, fMarketShare, fModelYear, fSales, fStock, fSurvival)


type SurvivalCube      = '[FClassification, FModelYear, FYear]          ↝ '[FSurvival]
type RegionalSalesCube = '[FRegion,                         FModelYear] ↝ '[FSales]
type MarketSharesCube  = '[FRegion,        FClassification, FModelYear] ↝ '[FMarketShare]
type SalesCube         = '[FRegion,        FModelYear, FClassification] ↝ '[FSales]
type StockCube         = '[FRegion, FYear, FClassification, FModelYear] ↝ '[FSales, FStock]
type StockCube'        = '[FRegion, FYear, FClassification, FModelYear] ↝ '[FSales, FSurvival]
type SalesStocksCube   = '[FRegion, FClassification, FYear]             ↝ '[FSales, FStock]


universe :: MarketSharesCube -> [FieldRec '[FRegion, FModelYear, FClassification, FYear]]
universe cube =
  [
    rmc <+> y
  |
    rmc <- projectKeys rcast                          cube :: [FieldRec '[FRegion, FModelYear, FClassification]]
  , y   <- projectKeys ((fYear =:) . (fModelYear <:)) cube
  , fModelYear <: rmc <= fYear <: y
  ]


-- FIXME: Throughout this module, use lens arithmetic to avoid all of the getting and setting.  The basic pattern can be 'rcast $ . . . lens arithmetic . . . $ mconcat [ . . . records providing field . . . ]'.
saleFromShare :: k -> FieldRec '[FSales, FMarketShare] -> FieldRec '[FSales]
saleFromShare _ rec = fSales =: (fSales <: rec * fMarketShare <: rec)


-- FIXME: Throughout this module, use lens arithmetic to avoid all of the getting and setting.  The basic pattern can be 'rcast $ . . . lens arithmetic . . . $ mconcat [ . . . records providing field . . . ]'.
sumSales :: k -> [FieldRec '[FSales, FSurvival]] -> FieldRec '[FSales, FStock]
sumSales _ xs = fSales =: (fSales <: last xs) <+> fStock =: sum ((\x -> fSales <: x * fSurvival <: x) <$> xs)


computeStock :: SurvivalCube
             -> RegionalSalesCube
             -> MarketSharesCube
             -> SalesStocksCube
computeStock survival regionalSales marketShares =
  let
    support = universe marketShares
    sales = π saleFromShare $ regionalSales ⋈  marketShares
    stock = κ support sumSales $ sales ⋈  survival
  in
      stock


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
