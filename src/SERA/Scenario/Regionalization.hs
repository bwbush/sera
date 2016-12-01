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


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Scenario.Regionalization (
-- * Types
  RegionalizationParameters(..)
-- * Functions
, travelReduction
, regionalize
, urbanToRegion
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.DataCube (Rekeyer(..), rekey)
import Data.Daft.Vinyl.FieldCube (type (↝), (!), (⋈), κ, π, σ, τ, ω)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Vinyl.Lens (type (∈))
import GHC.Generics (Generic)
import SERA.Scenario.Introduction (FIntroductionYear, fIntroductionYear, hasStations, RegionalIntroductionsCube)
import SERA.Types (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName, FYear, fYear, pushYear, minimumYear)
import SERA.Vehicle.Stock.Types (StockCube)
import SERA.Vehicle.Types (fEnergy, FRelativeMarketShare, fRelativeMarketShare, fSales, fStock, hasStock, fTravel, FVehicle, FVocation)


-- | Field type for total relative market share.
type FTotalRelativeMarketShare = '("Total Relative Market Share", Double)


-- | Field label for total relative market share.
fTotalRelativeMarketShare :: SField FTotalRelativeMarketShare
fTotalRelativeMarketShare = SField


-- | Total the relative market share.
totalRelativeMarketShare :: (FRelativeMarketShare ∈ vs)
                         => k                                     -- ^ The key for the data cube.
                         -> [FieldRec vs]                         -- ^ The values to be summarized.
                         -> FieldRec '[FTotalRelativeMarketShare] -- ^ The total of the relative market share.
totalRelativeMarketShare _ = (fTotalRelativeMarketShare =:) . sum . map (fRelativeMarketShare <:)


-- | Field type for travel reduction.
type FTravelReduction = '("Travel Reduction", Double)


-- | Field label for travel reduction.
fTravelReduction :: SField FTravelReduction
fTravelReduction = SField


-- | Parameters for regionalizing demand.
data RegionalizationParameters =
  RegionalizationParameters
  {
    initialTravelReduction  :: Double -- ^ Initial travel reduction.
  , travelReductionDuration :: Double -- ^ Number of years of travel reduction.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON RegionalizationParameters

instance ToJSON RegionalizationParameters


-- | Compute VMT reduction.
travelReduction :: RegionalizationParameters -- ^ Parameters for regionalizing demand.
                -> Double                    -- ^ The number of years since vehicles were introduced.
                -> Double                    -- ^ Multiplier for VMT.
travelReduction RegionalizationParameters{..} time
  | time < 0                       = initialTravelReduction
  | time > travelReductionDuration = 1
  | otherwise                      = initialTravelReduction + (1 - initialTravelReduction) * time / travelReductionDuration
-- | Regionalize demand.
regionalize :: RegionalizationParameters -- ^ Regionalization parameters.
            -> RegionalIntroductionsCube -- ^ Regional introduction years.
            -> StockCube                 -- ^ Total stock.
            -> StockCube                 -- ^ Regionalized stock.
regionalize parameters introductions totals = -- FIXME: Review for opportunities to simplify and clarify.
  let
    universe = ω totals :: Set (FieldRec '[FYear])
    firstYears :: '[FRegion, FVocation, FVehicle] ↝ '[FYear]
    firstYears =
        κ universe minimumYear
      $ π pushYear
      $ σ hasStock totals
    allocating key rec =
      let
        year = fYear <: key
        introductionYear = fIntroductionYear <: rec
        firstYear = (fYear <:) $ firstYears ! τ key
        year' = firstYear + year - maximum [introductionYear, firstYear]
        share = fRelativeMarketShare <: rec
        reduction = travelReduction parameters $ fromIntegral $ year' - introductionYear
      in
        fRelativeMarketShare =:
          (share
            * if year >= introductionYear
                then (fStock <:) $ totals ! (fYear =: year' <+> τ key)
                else 0
          )
            <+> fTravelReduction =: reduction
    allocations =
      π allocating
        $ σ pruneTooEarly
        $ σ hasStations introductions ⋈ totals
    universe' = ω allocations :: Set (FieldRec '[FUrbanCode, FUrbanName, FVocation, FVehicle])
    totals'' = κ universe' totalRelativeMarketShare allocations :: '[FRegion, FVocation, FVehicle, FYear] ↝ '[FTotalRelativeMarketShare]
    scaling _ rec =
      let
        share = fRelativeMarketShare <: rec / fTotalRelativeMarketShare <: rec
        reduction = fTravelReduction <: rec
      in
            fSales  =:             share * fSales   <: rec
        <+> fStock  =:             share * fStock   <: rec
        <+> fTravel =: reduction * share * fTravel  <: rec
        <+> fEnergy =: reduction * share * fEnergy  <: rec
  in
    urbanToRegion
      $ σ hasStock
      $ π scaling
      $ allocations ⋈ totals'' ⋈ totals


-- | Rekey with urban areas as the main key.
urbanToRegion :: '[FRegion, FUrbanCode, FUrbanName, FYear, FVocation, FVehicle] ↝ v -- ^ Data cube with both region and urban areas.
              -> '[FYear, FRegion, FVocation, FVehicle] ↝ v                         -- ^ Data cube with urban area as region.
urbanToRegion = -- Review for opportunities to simplify and clarify.
  rekey
    $ Rekeyer
        (\key -> τ $ fRegion =: Region (region (fRegion <: key) ++ " | " ++ urbanCode (fUrbanCode <: key) ++ " | " ++ urbanName (fUrbanName <: key)) <+> key)
        undefined


-- | Check whether the year is not earlier than the introduction year.
pruneTooEarly :: (FYear ∈ ks, FIntroductionYear ∈ vs)
              => FieldRec ks -- ^ The key.
              -> FieldRec vs -- ^ The value.
              -> Bool        -- ^ Whether the year is not earlier than the introduction year.
pruneTooEarly key rec = fYear <: key >= fIntroductionYear <: rec
