-----------------------------------------------------------------------------
--
-- Module      :  SERA.Scenario.Introduction
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Computing introduction years for vehicles.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Scenario.Introduction (
-- * Types
  StationParameters(..)
, CoverageParameters(..)
, StationCountParameters(..)
-- * Functions
, computeIntroductionYears
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldCube ((⋈), π, τ)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import GHC.Generics (Generic)
import SERA.Scenario.Types
import SERA.Vehicle.Types (fRelativeMarketShare, fStock)


-- | Parameters for computing number of stations for an urban area.
data StationParameters =
  StationParameters
  {
    coverageParameters           :: CoverageParameters     -- ^ Parameters for number of coverage stations.
  , threshholdStationsParameters :: StationCountParameters -- ^ Parameters for number of threshhold stations.
  , maximumStationsParameters    :: StationCountParameters -- ^ Parameters for maximum number of stations.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON StationParameters

instance ToJSON StationParameters


-- | Parameters for number of coverage stations.
data CoverageParameters =
  CoverageParameters
  {
    scaleFactor   :: Double -- ^ Scale factor.
  , densityFactor :: Double -- ^ Density factor.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON CoverageParameters

instance ToJSON CoverageParameters


-- | Estimate number of coverage stations.
coverageCount :: CoverageParameters -- ^ Coverage station parameters.
              -> Double             -- ^ Population density, per square mile.
              -> Double             -- ^ Area, in square miles.
              -> Double             -- ^ Number of coverage stations.
coverageCount CoverageParameters{..} density area =
  1 +  scaleFactor * exp(- densityFactor * density) * (area / 100)


-- | Parameters for number of stations.
data StationCountParameters =
  StationCountParameters
  {
    intercept :: Double -- ^ Intercept. 
  , slope     :: Double -- ^ Slope.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON StationCountParameters

instance ToJSON StationCountParameters


-- | Estimate number of stations.
stationCount :: StationCountParameters -- ^ Station count parameters.
              -> Double                -- ^ Population.
              -> Double                -- ^ Number of stations.
stationCount StationCountParameters{..} population =
  slope * population / 1000000 + intercept


-- | Compute introduction years.
computeIntroductionYears :: StationParameters                  -- ^ The station count parameters.
                         -> OverrideIntroductionYearsCube      -- ^ Which introduction years to override.
                         -> RegionalIntroductionParametersCube -- ^ Parameters for regional introdutions.
                         -> UrbanCharacteristicsCube           -- ^ Urban characteristics.
                         -> RegionalIntroductionsCube          -- ^ Regional introduction years.
computeIntroductionYears StationParameters{..} overrides regional urban =
  let
    introducing key rec =
      let
        introduce percentile = fFirstYear <: rec + (fLastYear <: rec - fFirstYear <: rec) * percentile / 100
        eam = fPercentileEAM <: rec / 100
        base = introduce $ fPercentileEAM <: rec
        nearby = introduce $ fNearbyPercentileEAM <: rec
        area = fArea <: rec / 1.60934^(2 :: Int)
        population = fPopulation <: rec
        density = population / area
        coverageStations = coverageCount coverageParameters density area
        clustering = fClustering <: rec
        delay = fDelay <: rec
        floor'   x = if isNaN population then 0 else floor   x
        ceiling' x = if isNaN population then 0 else ceiling x
      in
            fRelativeMarketShare =: fIntensification <: rec * fStock <: rec
        <+> fIntroductionYear    =: maybe
                                       (round (base * (1 - clustering) + nearby * clustering + delay * eam))
                                       (fIntroductionYear <:)
                                       (overrides `evaluate` τ key)
        <+> fStationCount        =: floor' (coverageStations / 3 + 1)
        <+> fCoverageStations    =: ceiling' coverageStations
        <+> fThreshholdStations  =: ceiling' (stationCount threshholdStationsParameters population)
        <+> fMaximumStations     =: ceiling' (stationCount maximumStationsParameters    population)
  in
    π introducing
      $ regional ⋈ urban
