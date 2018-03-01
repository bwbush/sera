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
-- * Cubes
, SalesOnlyCube
, TravelReductionCube
-- * Fields
, FTravelReduction
, fTravelReduction
-- * Functions
, travelReduction
, regionalize
, urbanToRegion
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.DataCube (Rekeyer(..), rekey)
import Data.Daft.Vinyl.FieldCube (type (↝), (!), (⋈), κ, π, σ, τ, ω)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Default.Util (nan)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Vinyl.Lens (type (∈))
import GHC.Generics (Generic)
import SERA.Scenario.Types (FIntroductionYear, fIntroductionYear, hasStations, fMaximumSales, RegionalIntroductionsCube)
import SERA.Types (pushYear, minimumYear)
import SERA.Types.Fields (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName, FYear, fYear)
import SERA.Vehicle.Stock.Types (StockCube)
import SERA.Vehicle.Types (FModelYear, fModelYear, FRelativeMarketShare, fRelativeMarketShare, FSales, fSales, hasStock, Vehicle(..), FVehicle, fVehicle, FVocation, fVocation)


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


type TravelReductionCube = '[FRegion, FYear, FVocation, FVehicle] ↝ '[FTravelReduction]


type SalesOnlyCube = '[FRegion, FModelYear, FVocation, FVehicle] ↝ '[FSales]


-- | Parameters for regionalizing demand.
data RegionalizationParameters =
  RegionalizationParameters
  {
    initialTravelReduction  :: Double -- ^ Initial travel reduction.
  , travelReductionDuration :: Double -- ^ Number of years of travel reduction.
  , logisticIntensification :: Maybe Double
  , shareIntensification    :: Maybe Double
  , salesLimit              :: Maybe Double
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON RegionalizationParameters

instance ToJSON RegionalizationParameters


-- | Compute VMT reduction.
travelReduction :: RegionalizationParameters -- ^ Parameters for regionalizing demand.
                -> Double                    -- ^ The number of years since vehicles were introduced.
                -> Double                    -- ^ Fraction reduction for VMT.
travelReduction RegionalizationParameters{..} time
  | time < 0                       = initialTravelReduction
  | time > travelReductionDuration = 0
  | otherwise                      = initialTravelReduction * (1 - time / travelReductionDuration)
-- | Regionalize demand.
regionalize :: RegionalizationParameters            -- ^ Regionalization parameters.
            -> RegionalIntroductionsCube            -- ^ Regional introduction years.
            -> StockCube                            -- ^ Total stock.
            -> (SalesOnlyCube, TravelReductionCube) -- ^ Regionalized sales and VMT reductions.
regionalize parameters introductions totals = -- FIXME: Review for opportunities to simplify and clarify.
  let
    totals' = σ (\key _ -> fVehicle <: key == Vehicle "FCEV") totals
    universe = ω totals' :: Set (FieldRec '[FYear])
    firstYears :: '[FRegion, FVocation, FVehicle] ↝ '[FYear]
    firstYears =
        κ universe minimumYear
      $ π pushYear
      $ σ hasStock totals'
    allocating key rec =
      let
        year = fYear <: key
        introductionYear = fIntroductionYear <: rec
        firstYear = (fYear <:) $ firstYears ! τ key
        year' = firstYear + year - maximum [introductionYear, firstYear]
        share = (fRelativeMarketShare <: rec) ** fromMaybe 1 (shareIntensification parameters)
        reduction = travelReduction parameters $ fromIntegral $ year' - firstYear
      in
        fRelativeMarketShare =:
          (share
            * if year >= introductionYear
                then ((fSales <:) $ totals' ! (fYear =: year' <+> τ key)) ** fromMaybe 1 (logisticIntensification parameters)
                else 0
          )
            <+> fTravelReduction =: reduction
            <+> fMaximumSales =: fMaximumSales <: rec
    allocations =
      π allocating
        $ σ pruneTooEarly
        $ σ hasStations introductions ⋈ totals'
    universe' = ω allocations :: Set (FieldRec '[FUrbanCode, FUrbanName, FVocation, FVehicle])
    totals'' = κ universe' totalRelativeMarketShare allocations :: '[FRegion, FVocation, FVehicle, FYear] ↝ '[FTotalRelativeMarketShare]
    scaling _ rec =
      let
        share = fRelativeMarketShare <: rec / fTotalRelativeMarketShare <: rec
        sales = share * fSales <: rec
        maximumSales = maybe nan (* (fMaximumSales <: rec)) $ salesLimit parameters
      in
        fSales =: if isNaN maximumSales || sales <= maximumSales then sales else maximumSales
  in
    (
      urbanToRegion
        $ π scaling
        $ allocations ⋈ totals'' ⋈ totals'
    , urbanToRegion'
        $ π (const τ)
        allocations
    )


-- | Rekey with urban areas as the main key.
urbanToRegion :: '[FRegion, FUrbanCode, FUrbanName, FYear, FVocation, FVehicle] ↝ v -- ^ Data cube with both region and urban areas.
              -> '[FRegion, FModelYear, FVocation, FVehicle] ↝ v                         -- ^ Data cube with urban area as region.
urbanToRegion = -- Review for opportunities to simplify and clarify.
  rekey
    $ Rekeyer
        (\key ->     fRegion    =: Region (region (fRegion <: key) ++ " | " ++ urbanCode (fUrbanCode <: key) ++ " | " ++ urbanName (fUrbanName <: key))
                 <+> fModelYear =: fYear     <: key
                 <+> fVocation  =: fVocation <: key
                 <+> fVehicle   =: fVehicle  <: key
        )
        undefined




-- | Rekey with urban areas as the main key.
urbanToRegion' :: '[FRegion, FUrbanCode, FUrbanName, FYear, FVocation, FVehicle] ↝ v -- ^ Data cube with both region and urban areas.
               -> '[FRegion, FYear, FVocation, FVehicle] ↝ v                         -- ^ Data cube with urban area as region.
urbanToRegion' = -- Review for opportunities to simplify and clarify.
  rekey
    $ Rekeyer
        (\key ->     fRegion    =: Region (region (fRegion <: key) ++ " | " ++ urbanCode (fUrbanCode <: key) ++ " | " ++ urbanName (fUrbanName <: key))
                 <+> fYear      =: fYear     <: key
                 <+> fVocation  =: fVocation <: key
                 <+> fVehicle   =: fVehicle  <: key
        )
        undefined


-- | Check whether the year is not earlier than the introduction year.
pruneTooEarly :: (FYear ∈ ks, FIntroductionYear ∈ vs)
              => FieldRec ks -- ^ The key.
              -> FieldRec vs -- ^ The value.
              -> Bool        -- ^ Whether the year is not earlier than the introduction year.
pruneTooEarly key rec = fYear <: key >= fIntroductionYear <: rec
