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


import Data.Daft.DataCube (Rekeyer(..), rekey)
import Data.Daft.Vinyl.FieldCube (type (↝), (!), (⋈), κ, π, σ, τ, ω)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec, SField(..))
import SERA.Scenario.Introduction (FCoverageStations, FIntroductionYear, fIntroductionYear, FMaximumStations, fMaximumStations, RegionalIntroductionsCube, FStationCount, FThreshholdStations)
import SERA.Types (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName, FYear, fYear)
import SERA.Vehicle.Stock.Types (StockCube)
import SERA.Vehicle.Types (FEnergy, fEnergy, FRelativeMarketShare, fRelativeMarketShare, FSales, fSales, FStock, fStock, FTravel, fTravel, FVehicle, FVocation)


type FTotalRelativeMarketShare = '("Total Relative Market Share", Double)

fTotalRelativeMarketShare :: SField FTotalRelativeMarketShare
fTotalRelativeMarketShare = SField


totalRelativeMarketShare :: k -> [FieldRec '[FRelativeMarketShare]] -> FieldRec '[FTotalRelativeMarketShare]
totalRelativeMarketShare _ = (fTotalRelativeMarketShare =:) . sum . map (fRelativeMarketShare <:)


hasStations :: k -> FieldRec '[FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations] -> Bool
hasStations = const $ (/= 0) . (fMaximumStations <:)


hasSales :: k -> FieldRec '[FSales, FStock, FTravel, FEnergy] -> Bool
hasSales = const $ (/= 0) . (fStock <:)


urbanToRegion :: '[FRegion, FUrbanCode, FUrbanName, FYear, FVocation, FVehicle] ↝ v -> '[FYear, FRegion, FVocation, FVehicle] ↝ v
urbanToRegion =
  rekey
    $ Rekeyer
        (\key -> τ $ fRegion =: Region (region (fRegion <: key) ++ " | " ++ urbanCode (fUrbanCode <: key) ++ " | " ++ urbanName (fUrbanName <: key)) <+> key)
        undefined


pushYear :: FieldRec '[FYear, FRegion, FVocation, FVehicle] -> v -> FieldRec '[FYear]
pushYear key _ = τ key


minimumYear  :: k -> [FieldRec '[FYear]] -> FieldRec '[FYear]
minimumYear _ recs = fYear =: minimum ((fYear <:) <$> recs)


pruneTooEarly :: FieldRec '[FRegion, FUrbanCode, FUrbanName, FYear, FVocation, FVehicle] -> FieldRec '[FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations, FSales, FStock, FTravel, FEnergy] -> Bool
pruneTooEarly key rec = fYear <: key >= fIntroductionYear <: rec


regionalize :: RegionalIntroductionsCube -> StockCube -> StockCube
regionalize introductions totals =
  let
    universe = ω totals :: Set (FieldRec '[FYear])
    firstYears :: '[FRegion, FVocation, FVehicle] ↝ '[FYear]
    firstYears =
        κ universe minimumYear
      $ π pushYear
      $ σ hasSales totals
    allocating :: FieldRec '[FRegion, FUrbanCode, FUrbanName, FYear, FVocation, FVehicle] -> FieldRec '[FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations, FSales, FStock, FTravel, FEnergy] -> FieldRec '[FRelativeMarketShare]
    allocating key rec =
      let
        year = fYear <: key
        introductionYear = fIntroductionYear <: rec
        firstYear = (fYear <:) $ firstYears ! τ key
        year' = firstYear + year - maximum [introductionYear, firstYear]
        share = fRelativeMarketShare <: rec
      in
        fRelativeMarketShare =:
          share
            * if year >= introductionYear
                then (fStock <:) $ totals ! (fYear =: year' <+> τ key)
                else 0
    allocations =
      π allocating
        $ σ pruneTooEarly
        $ (σ hasStations introductions) ⋈ totals
    universe' = ω allocations :: Set (FieldRec '[FUrbanCode, FUrbanName, FVocation, FVehicle])
    totals'' = κ universe' totalRelativeMarketShare allocations :: '[FRegion, FVocation, FVehicle, FYear] ↝ '[FTotalRelativeMarketShare]
    scaling :: FieldRec '[FRegion, FUrbanCode, FUrbanName, FYear, FVocation, FVehicle] -> FieldRec '[FRelativeMarketShare, FTotalRelativeMarketShare, FSales, FStock, FTravel, FEnergy] -> FieldRec '[FSales, FStock, FTravel, FEnergy]
    scaling _ rec =
      let
        share = fRelativeMarketShare <: rec / fTotalRelativeMarketShare <: rec
      in
            fSales  =: share * fSales   <: rec
        <+> fStock  =: share * fStock   <: rec
        <+> fTravel =: share * fTravel  <: rec
        <+> fEnergy =: share * fEnergy  <: rec
  in
    urbanToRegion
      $ σ hasSales
      $ π scaling
      $ allocations ⋈ totals'' ⋈ totals
