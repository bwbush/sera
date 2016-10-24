-----------------------------------------------------------------------------
--
-- Module      :  SERA.Scenario.Logistic
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Logistic-curve computations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}


module SERA.Scenario.Logistic (
-- * Types
  LogisticCube
, LogisticParameters(..)
-- * Functions
, applyLogistic
, share
) where


import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Daft.Vinyl.FieldCube (type (↝), (⋈), π, fromRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Vinyl.Derived (FieldRec, SField(..))
import SERA.Types (FRegion)
import SERA.Vehicle.Types (MarketShare, FMarketShare, fMarketShare, ModelYear, FModelYear, fModelYear, FVehicle, FVocation)
import SERA.Vehicle.Stock.Types (MarketShareCube, ModelYearCube)


data LogisticParameters =
  LogisticParameters
    {
      m    :: Double
    , r    :: Double
    , beta :: Double
    , s0   :: Double
    , t0   :: Double
    }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON LogisticParameters

instance ToJSON LogisticParameters where


type FReferenceYear = '("Reference Year", ModelYear)


fReferenceYear :: SField FReferenceYear
fReferenceYear = SField


type FReferenceShare = '("Reference Share [veh/veh]", MarketShare)


fReferenceShare :: SField FReferenceShare
fReferenceShare = SField


type FMaximumShare = '("Maximum Share [veh/veh]", MarketShare)


fMaximumShare :: SField FMaximumShare
fMaximumShare = SField


type FGrowthRate = '("Growth Rate [/yr]", Double)


fGrowthRate :: SField FGrowthRate
fGrowthRate = SField


type FTimeScaling = '("Time Scaling [yr/yr]", Double)


fTimeScaling :: SField FTimeScaling
fTimeScaling = SField


type LogisticCube = '[FRegion, FVocation, FVehicle] ↝ '[FReferenceYear, FReferenceShare, FMaximumShare, FGrowthRate, FTimeScaling]


share :: LogisticParameters -> Int -> Double
share LogisticParameters{..} year = m / (1 + (m / s0 - 1) * exp (- r * m * (beta * fromIntegral year - t0)))


sharing :: FieldRec '[FRegion, FVocation, FVehicle, FModelYear] -> FieldRec '[FReferenceYear, FReferenceShare, FMaximumShare, FGrowthRate, FTimeScaling] -> FieldRec '[FMarketShare]
sharing key rec =
  let
    t    =                fModelYear      <: key
    t0   = fromIntegral $ fReferenceYear  <: rec
    s0   =                fReferenceShare <: rec
    m    =                fMaximumShare   <: rec
    r    = (/ m)        $ fGrowthRate     <: rec
    beta =                fTimeScaling    <: rec
  in
    fMarketShare =: share LogisticParameters{..} t


applyLogistic :: (ModelYear, ModelYear) -> LogisticCube -> MarketShareCube
applyLogistic (firstYear, lastYear) logistics =
  let
    modelYears :: ModelYearCube
    modelYears = fromRecords [fModelYear =: y | y <- [firstYear..lastYear]]
  in
    π sharing
      $ logistics ⋈ modelYears
