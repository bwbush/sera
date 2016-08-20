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

{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module SERA.Vehicle.Stock (
  inferSales
) where


import Control.Arrow ((&&&))
import Control.Parallel.Strategies (rpar)
import Data.List.Util (replicateHead)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Daft.MapReduce.Parallel (aggregate, groupExtract, groupReduceFlattenByKey)
import Data.Matrix (fromList, inverse, matrix, multStd, toList)
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived (FieldRec, (=:))
import Data.Vinyl.Lens (rcast)
import SERA (trace')
import SERA.Types (FRegion, Year, fYear)
import SERA.Vehicle.Stock.Types (SalesStockRecord, StockRecord, SurvivalFunction)
import SERA.Vehicle.Types (Classification, Sales, Stock, FClassification, fClassification, fSales, fStock)


-- | Infer sales from stock.
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
