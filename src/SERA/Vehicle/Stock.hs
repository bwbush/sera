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
{-# LANGUAGE RecordWildCards           #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module SERA.Vehicle.Stock (
  computeStock
, inferSales
, inferMarketShares
) where


import Control.Arrow ((&&&))
import Data.Daft.DataCube
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.List.Util (replicateHead)
import Data.Proxy (Proxy(..))
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec, (=:))
import Data.Vinyl.Lens (rcast)
import SERA (trace')
import SERA.Types (FRegion, FYear, Year, fYear)
import SERA.Vehicle.Stock.Types (MarketSharesRecord, NewVehiclesRecord, SalesStockRecord, StockRecord, SurvivalFunction)
import SERA.Vehicle.Types (Classification, FAge, FClassification, FMarketShare, FModelYear, FSales, FStock, FSurvival, Sales, Stock, fAge, fClassification, fMarketShare, fModelYear, fSales, fStock, fSurvival)


type SurvivalCube = FieldCube '[FClassification, FAge] '[FSurvival]

type NewVehiclesCube = FieldCube '[FRegion, FModelYear] '[FSales]

type MarketSharesCube = FieldCube '[FRegion, FClassification, FModelYear] '[FMarketShare]

type SalesCube = FieldCube '[FRegion, FClassification, FModelYear] '[FSales]

type StockCube = FieldCube '[FRegion, FClassification, FYear] '[FStock]

type SalesStocksCube = FieldCube '[FRegion, FClassification, FYear] '[FSales, FStock]


saleFromShare :: FieldRec '[FSales, FMarketShare] -> FieldRec '[FSales]
saleFromShare rec = fSales =: (fSales <: rec * fMarketShare <: rec)


modelYearToYear :: FieldCube '[FRegion, FClassification, FModelYear] v -> FieldCube '[FRegion, FClassification, FYear] v
modelYearToYear =
  rekey Rekeyer{..}
    where
      rekeyer   rec = rcast $ rec <+> fYear      =: (fModelYear <: rec)
      unrekeyer rec = rcast $ rec <+> fModelYear =: (fYear      <: rec)


joinByAge = undefined


computeStock :: SurvivalCube
             -> NewVehiclesCube
             -> MarketSharesCube
             -> SalesStocksCube
computeStock survival newVehicles marketShares =
  let
    sales = π (const saleFromShare) $ newVehicles ⋈  marketShares :: SalesCube
    stock = undefined :: StockCube
  in
    modelYearToYear sales ⋈  stock
{-
    computeForRegionClassification :: FieldRec '[FRegion, FClassification] -> [FieldRec '[FRegion, FClassification, FModelYear, FSales]] -> [SalesStockRecord]
    computeForRegionClassification regionClassification classifiedSales =
      let
        classification = fClassification <: regionClassification
        stock []               = []
        stock ss@(_ : ss') = sum (zipWith (\x y -> x * survival classification y) ss [0..]) : stock ss'
        recs = groupExtract rpar (fModelYear <:) id classifiedSales
        sales = map (fSales <:) recs
      in
        [
          rcast $ rec <+> fStock =: y <+> fYear =: (fModelYear <: rec)
        |
          (rec, y) <- zip recs . reverse . stock $ reverse sales
        ]
  in
    groupReduceFlattenByKey rpar rcast computeForRegionClassification
      [
        let
          sale = fSales <: rec * fMarketShare <: rec
        in
          rcast $ rec <+> fSales =: sale
      |
        rec <- naturalJoin (Proxy :: Proxy '[FRegion, FModelYear]) newVehicles marketShares
            :: [FieldRec '[FRegion, FClassification, FModelYear, FSales, FMarketShare]]
      ]
-}


-- | Infer vehicle sales from vehicle stock.
inferSales :: Int                -- ^ Number of prior years to generate sales for the stock.
           -> SurvivalFunction   -- ^ The vehicle survival function.
           -> [StockRecord]      -- ^ The vehicle stock records.
           -> [SalesStockRecord] -- ^ The vehicle sales and stock records.
inferSales padding survival =
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
