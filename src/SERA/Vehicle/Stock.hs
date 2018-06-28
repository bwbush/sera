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
, recomputeStock
) where


import Control.Applicative (liftA2)
import Data.Daft.DataCube
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec)
import SERA.Scenario.Regionalization (PurchasesOnlyCube, TravelReductionCube, fTravelReduction)
import SERA.Types.Fields (FYear, fYear, FAge, fAge, fAnnualTravel, fEmission, fEmissionRate, fEnergy, FFuel, fFuelEfficiency, fFuelSplit, fMarketShare, FModelYear, fModelYear, fPurchases, fStock, fSurvival, fTravel, FVehicle, FVocation, FWilderRegion)
import SERA.Types.Cubes (AnnualTravelCube, EmissionRateCube, EmissionCube, EnergyCube, FuelEfficiencyCube, FuelSplitCube, MarketShareCube, RegionalPurchasesCube, PurchasesCube, StockCube, SurvivalCube, wilderRegions)
import SERA.Util.Wilder

import qualified Data.Set as S (findMin, map)


-- | Add calendar year to a key.
byYear :: '[FWilderRegion, FModelYear, FVocation, FVehicle, FAge] ↝ v  -- ^ The data cube.
       -> '[FYear, FWilderRegion, FVocation, FVehicle, FModelYear] ↝ v -- ^ The data cube year added to the key.
byYear =
  rekey Rekeyer{..}
    where
      rekeyer   rec = τ (rec <+> fYear =: tame (liftA2 (+) (fAge  <: rec) (fModelYear <: rec)))
      unrekeyer rec = τ (rec <+> fAge  =: fmap (+ negate (fYear <: rec))  (fModelYear <: rec))


-- | Compute the vehicle stock.
computeStock :: RegionalPurchasesCube                                 -- ^ Regional sales.
             -> MarketShareCube                                   -- ^ Market share.
             -> SurvivalCube                                      -- ^ Vehicle survival.
             -> AnnualTravelCube                                  -- ^ Annual travel.
             -> FuelSplitCube                                     -- ^ Fuel split.
             -> FuelEfficiencyCube                                -- ^ Fuel efficiency.
             -> EmissionRateCube                                  -- ^ Emission rate.
             -> (PurchasesCube, StockCube, EnergyCube, EmissionCube)  -- ^ Purchases, stock, energy consumed, and pollutants emitted.
computeStock regionalPurchases marketShares survival annualTravel fuelSplit fuelEfficiency emissionRate = -- FIXME: Review for opportunities to simplify.
  let
    modelYears = ω regionalPurchases :: Set (FieldRec '[FModelYear])
    firstYear = fModelYear <: S.findMin modelYears
    years = ((fYear =:) . tame . (fModelYear <:)) `S.map` modelYears
    ages = ((fAge =:) . liftA2 (-) firstYear . (fModelYear <:)) `S.map`  modelYears
    fuels = ω fuelEfficiency :: Set (FieldRec '[FFuel])
    traveling key rec =
      let
        sales = fPurchases <: rec * fMarketShare <: rec
        sales' = if fAge <: key == Tame 0 then sales else 0
        stock = sales * fSurvival <: rec
        travel' = stock * fAnnualTravel <: rec
      in
            fPurchases  =: sales'
        <+> fStock  =: stock
        <+> fTravel =: travel'
    travel =
      ρ (years × ω marketShares)
      $ byYear
      (
        π traveling
        $ regionalPurchases
        ⋈ marketShares
        ⋈ survival
        ⋈ annualTravel
      )
    consuming _ rec =
      let
        split = fFuelSplit <: rec
        sales = split * fPurchases <: rec
        stock = split * fStock <: rec
        travel' = split * fTravel <: rec
        energy' = travel' / fFuelEfficiency <: rec
      in
            fPurchases  =: sales
        <+> fStock  =: stock
        <+> fTravel =: travel'
        <+> fEnergy =: energy'
    energy =
      π consuming
      $ ε travel
      ⋈ fuelSplit
      ⋈ fuelEfficiency
    emitting _ rec =
      fEmission =: fEnergy <: rec * fEmissionRate <: rec
    emission =
      π emitting
      $ energy
      ⋈ emissionRate
    total _ xs =
          fPurchases  =: sum ((fPurchases  <:) <$> xs)
      <+> fStock  =: sum ((fStock  <:) <$> xs)
      <+> fTravel =: sum ((fTravel <:) <$> xs)
      <+> fEnergy =: sum ((fEnergy <:) <$> xs)
    totalEmission _ xs =
          fEmission =: sum ((fEmission <:) <$> xs)
  in
    (
      κ (             ages × fuels) total           energy
    , κ (modelYears × ages × fuels) total           energy
    , κ (modelYears × ages        ) ((τ .) . total) energy
    , κ (modelYears × ages        ) totalEmission   emission
    )


-- | Compute the vehicle stock.
recomputeStock :: PurchasesOnlyCube                                     -- ^ Purchases.
               -> TravelReductionCube                               -- ^ Travel reduction.
               -> SurvivalCube                                      -- ^ Vehicle survival.
               -> AnnualTravelCube                                  -- ^ Annual travel.
               -> FuelSplitCube                                     -- ^ Fuel split.
               -> FuelEfficiencyCube                                -- ^ Fuel efficiency.
               -> EmissionRateCube                                  -- ^ Emission rate.
               -> (PurchasesCube, StockCube, EnergyCube, EmissionCube)  -- ^ Purchases, stock, energy consumed, and pollutants emitted.
recomputeStock regionalPurchases' travelReduction survival annualTravel fuelSplit fuelEfficiency emissionRate = -- FIXME: Review for opportunities to simplify.
  let
    regionalPurchases = wilderRegions regionalPurchases'
    modelYears = ω regionalPurchases :: Set (FieldRec '[FModelYear])
    firstYear = fModelYear <: (S.findMin modelYears)
    years = ((fYear =:) . tame . (fModelYear <:)) `S.map` modelYears
    ages = ((fAge =:) . (liftA2 (-) firstYear) . (fModelYear <:)) `S.map`  modelYears
    fuels = ω fuelEfficiency :: Set (FieldRec '[FFuel])
    traveling key rec =
      let
        sales = fPurchases <: rec
        sales' = if fAge <: key == Tame 0 then sales else 0
        stock = sales * fSurvival <: rec
        travel' = stock * fAnnualTravel <: rec
      in
            fPurchases  =: sales'
        <+> fStock  =: stock
        <+> fTravel =: travel'
    travel =
      ρ (years × ω regionalPurchases)
      $ byYear
      (
        π traveling
        $ regionalPurchases
        ⋈ survival
        ⋈ annualTravel
      )
    consuming _ rec =
      let
        split = fFuelSplit <: rec
        sales = split * fPurchases <: rec
        stock = split * fStock <: rec
        travel' = split * fTravel <: rec
        energy' = travel' / fFuelEfficiency <: rec
        reduction = fTravelReduction <: rec
      in
            fPurchases  =: sales
        <+> fStock  =: stock
        <+> fTravel =: travel' * (1 - reduction)
        <+> fEnergy =: energy' * (1 - reduction)
    energy =
      π consuming
      $ (⋈ wilderRegions travelReduction)
      $ ε travel
      ⋈ fuelSplit
      ⋈ fuelEfficiency
    emitting _ rec =
      fEmission =: fEnergy <: rec * fEmissionRate <: rec
    emission =
      π emitting
      $ energy
      ⋈ emissionRate
    total _ xs =
          fPurchases  =: sum ((fPurchases  <:) <$> xs)
      <+> fStock  =: sum ((fStock  <:) <$> xs)
      <+> fTravel =: sum ((fTravel <:) <$> xs)
      <+> fEnergy =: sum ((fEnergy <:) <$> xs)
    totalEmission _ xs =
          fEmission =: sum ((fEmission <:) <$> xs)
  in
    (
      κ (             ages × fuels) total           energy
    , κ (modelYears × ages × fuels) total           energy
    , κ (modelYears × ages        ) ((τ .) . total) energy
    , κ (modelYears × ages        ) totalEmission   emission
    )
