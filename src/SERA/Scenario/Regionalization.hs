-----------------------------------------------------------------------------
--
-- Module      :  SERA.Scenario.Regionalization
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Computing regionalization of vehicle demand.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}


module SERA.Scenario.Regionalization (
-- * Functions
  regionalize
) where


import Data.Daft.Vinyl.FieldCube (type (↝), (!), (⋈), κ, π, σ, ω)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec, SField(..))
import SERA.Scenario.Introduction (FIntroductionYear, fIntroductionYear, RegionalIntroductionsCube, FStationCount)
import SERA.Types (FRegion)
import SERA.Vehicle.Stock.Types (RegionalSalesCube)
import SERA.Vehicle.Types (FModelYear, fModelYear, FRelativeMarketShare, fRelativeMarketShare, FSales, fSales)

import qualified Data.Set as S


type FTotalRelativeMarketShare = '("Total Relative Market Share", Double)

fTotalRelativeMarketShare :: SField FTotalRelativeMarketShare
fTotalRelativeMarketShare = SField


totalSales :: k -> [FieldRec '[FSales]] -> FieldRec '[FSales]
totalSales _ = (fSales =:) . sum . map (fSales <:)


totalRelativeMarketShare :: k -> [FieldRec '[FRelativeMarketShare]] -> FieldRec '[FTotalRelativeMarketShare]
totalRelativeMarketShare _ = (fTotalRelativeMarketShare =:) . sum . map (fRelativeMarketShare <:)


hasSales :: k -> FieldRec '[FSales] -> Bool
hasSales = const $ (/= 0) . (fSales <:)


regionalize :: RegionalIntroductionsCube -> RegionalSalesCube -> RegionalSalesCube
regionalize introductions totals =
  let
    universe = ω totals :: Set (FieldRec '[FRegion])
    totals' = κ universe totalSales totals :: '[FModelYear] ↝ '[FSales]
    years = ω totals :: Set (FieldRec '[FModelYear])
    firstYear = fModelYear <: S.findMin years
    allocating :: FieldRec '[FRegion, FModelYear] -> FieldRec '[FRelativeMarketShare, FIntroductionYear, FStationCount, FSales] -> FieldRec '[FRelativeMarketShare]
    allocating key rec =
      let
        modelYear = fModelYear <: key
        introductionYear = fIntroductionYear <: rec
        year' = firstYear + modelYear - maximum [introductionYear, firstYear]
        share = fRelativeMarketShare <: rec
      in
        fRelativeMarketShare =:
          share
            * if modelYear >= introductionYear
                then (fSales <:) $ totals' ! (fModelYear =: year')
                else 0
    allocations = π allocating $ introductions ⋈ totals'
    universe' = ω allocations :: Set (FieldRec '[FRegion])
    totals'' = κ universe' totalRelativeMarketShare allocations :: '[FModelYear] ↝ '[FTotalRelativeMarketShare]
    scaling :: FieldRec '[FRegion, FModelYear] -> FieldRec '[FRelativeMarketShare, FTotalRelativeMarketShare, FSales] -> FieldRec '[FSales]
    scaling _ rec =
      let
        share = fRelativeMarketShare <: rec / fTotalRelativeMarketShare <: rec
        sales = fSales <: rec
      in
        fSales =: share * sales
  in
    σ hasSales
      $ π scaling
      $ allocations ⋈ totals'' ⋈ totals'
