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

{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module SERA.Vehicle.Stock (
  computeStock
, inferSales
, inferMarketShares
) where


import Control.Arrow ((&&&))
import Control.Parallel.Strategies (rpar)
import Data.List.Util (replicateHead)
import Data.Daft.Vinyl.FieldRec ((<:), naturalJoin)
import Data.Daft.MapReduce.Parallel (aggregate, groupExtract, groupReduceByKey, groupReduceFlattenByKey)
import Data.Matrix (fromList, inverse, matrix, multStd, toList)
import Data.Proxy (Proxy(..))
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec, (=:))
import Data.Vinyl.Lens (rcast)
import SERA (trace')
import SERA.Types (FRegion, FYear, Year, fYear)
import SERA.Vehicle.Stock.Types (MarketSharesRecord, NewVehiclesRecord, SalesStockRecord, StockRecord, SurvivalFunction)
import SERA.Vehicle.Types (Classification, FClassification, FMarketShare, FModelYear, FSales, Sales, Stock, fClassification, fMarketShare, fModelYear, fSales, fStock)


computeStock :: SurvivalFunction
             -> [NewVehiclesRecord]
             -> [MarketSharesRecord]
             -> [SalesStockRecord]
computeStock survival newVehicles marketShares =
  let
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


-- | Infer vehicle sales from vehicle stock.
inferSales :: Int                -- ^ Number of prior years to generate sales for the stock.
           -> SurvivalFunction   -- ^ The vehicle survival function.
           -> [StockRecord]      -- ^ The vehicle stock records.
           -> [SalesStockRecord] -- ^ The vehicle sales and stock records.
inferSales padding survival =
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


-- | Infer regional total sales and market shares from vehicle sales.
inferMarketShares :: [SalesStockRecord]                          -- ^ The vehicle sales and stock records.
                  -> ([NewVehiclesRecord], [MarketSharesRecord]) -- ^ The regional total sales and market shares.
inferMarketShares =
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


-- | Invert a survival function, using matrix inversion.
inverseSurvivalFunction' :: SurvivalFunction -- ^ The survival function.
                         -> Classification   -- ^ The vehicles being classified.
                         -> [Year]           -- ^ The years for which to invert.
                         -> [Stock]          -- ^ The vehicle stock to be inverted.
                         -> [Sales]          -- ^ The vehicle sales.
inverseSurvivalFunction' survival classification years =
  let
    n = length years
    m = matrix n n $ \(i, j) -> if i >= j then survival classification $ i - j else 0
    Right mi = -- FIXME: Use a fast inversion formula for lower triangular matrices.
      trace' ("Computing sales from stock for \"" ++ show classification ++ "\".")
        $ inverse m
  in
    toList . multStd mi . fromList n 1


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
