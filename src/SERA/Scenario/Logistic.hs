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


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Scenario.Logistic (
-- * Types
  LogisticParameters(..)
-- * Functions
, marketShare
, computeMarketShares
) where


import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Daft.Vinyl.FieldCube ((⋈), π, fromRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import SERA.Scenario.Types
import SERA.Vehicle.Types (fMarketShare, ModelYear, fModelYear)
import SERA.Vehicle.Stock.Types (MarketShareCube, ModelYearCube)


-- | Logistic curve parameters.
data LogisticParameters =
  LogisticParameters
    {
      m    :: Double -- ^ Maximum share.
    , r    :: Double -- ^ Growth rate.
    , beta :: Double -- ^ Time scaling.
    , s0   :: Double -- ^ Reference share.
    , t0   :: Double -- ^ Reference year.
    }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON LogisticParameters

instance ToJSON LogisticParameters where


-- | Logistic curve.
marketShare :: LogisticParameters -- ^ Parameters defining the shape of the curve.
            -> Int                -- ^ The year.
            -> Double             -- ^ The market share in the given year.
marketShare LogisticParameters{..} year = m / (1 + (m / s0 - 1) * exp (- r * m * (beta * fromIntegral year - t0)))


-- | Compute market shares.
computeMarketShares :: (ModelYear, ModelYear) -- ^ The first and last model years to compute.
                    -> LogisticCube           -- ^ The logistic parameters.
                    -> MarketShareCube        -- ^ The market shares.
computeMarketShares (firstYear, lastYear) logistics =
  let
    modelYears = fromRecords [fModelYear =: y | y <- [firstYear..lastYear]] :: ModelYearCube
    sharing key rec =
      let
        t    =                fModelYear      <: key
        t0   = fromIntegral $ fReferenceYear  <: rec
        s0   =                fReferenceShare <: rec
        m    =                fMaximumShare   <: rec
        r    = (/ m)        $ fGrowthRate     <: rec
        beta =                fTimeScaling    <: rec
      in
        fMarketShare =: marketShare LogisticParameters{..} t
  in
    π sharing
      $ logistics ⋈ modelYears
