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
  SalesCube
, ShareCube
, LogisticParameters(..)
-- * Functions
, applyLogistic
, marketShare
, share
) where


import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Daft.DataCube (fromFunction)
import Data.Daft.Vinyl.FieldCube (type (↝), (⋈), π, ρ, ω)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Vinyl.Derived (FieldRec)
import SERA.Vehicle.Types (FMarketShare, fMarketShare, FModelYear, fModelYear, FSales, fSales)


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


share :: LogisticParameters -> Int -> Double
share LogisticParameters{..} year = m / (1 + (m / s0 - 1) * exp (- r * m * (beta * fromIntegral year - t0)))


-- | Market share as a function of model year.
type ShareCube = '[FModelYear] ↝ '[FMarketShare]


-- | Logistic function for market shares.
marketShare :: LogisticParameters -> ShareCube
marketShare parameters =
  fromFunction $ \rec ->
    return
      $ fMarketShare =: share parameters (fModelYear <: rec)


-- | Vehicle sales as a function of model year.
type SalesCube  = '[FModelYear] ↝ '[FSales]


sharing :: FieldRec '[FModelYear] -> FieldRec '[FMarketShare, FSales] -> FieldRec '[FSales]
sharing _ rec = fSales =: fMarketShare <: rec * fSales <: rec


applyLogistic :: LogisticParameters -> SalesCube -> SalesCube
applyLogistic parameters sales =
  ρ (ω sales)
    $ π sharing
    (marketShare parameters ⋈ sales)
