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


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}


module SERA.Scenario.Introduction (
-- * Types
  IntroductionParameters(..)
, UrbanCharacteristicsCube
, RegionalIntroductionsCube
, IntroductionYear
, FIntroductionYear
, fIntroductionYear
-- * Functions
, introductionYears
) where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.DataCube (Rekeyer(..), rekey)
import Data.Daft.Vinyl.FieldCube (type (↝), π, σ)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Default.Util (nan)
import Data.Maybe (fromMaybe)
import Data.Vinyl.Derived (FieldRec, SField(..))
import GHC.Generics (Generic)
import SERA.Types (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName)
import SERA.Vehicle.Types (FAnnualTravel, fAnnualTravel, FRelativeMarketShare, fRelativeMarketShare, FStock, fStock)


data IntroductionParameters =
  IntroductionParameters
  {
    clustering            :: Double
  , delays                :: [(Region, Double)]
  , shareIntensifications :: [(Region, Double)]
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON IntroductionParameters

instance ToJSON IntroductionParameters


type FBaseYear = '("Introduction Year - Base", Double)

fBaseYear :: SField FBaseYear
fBaseYear = SField


type FNearbyYear = '("Nearby Introduction", Double)

fNearbyYear :: SField FNearbyYear
fNearbyYear = SField


type FPercentileEAM = '("EAM Percentile", Double)

fPercentileEAM :: SField FPercentileEAM
fPercentileEAM = SField


type IntroductionYear = Int

type FIntroductionYear = '("Introduction Year", IntroductionYear)

fIntroductionYear :: SField FIntroductionYear
fIntroductionYear = SField


type UrbanCharacteristicsCube = '[FRegion, FUrbanCode, FUrbanName] ↝ '[FBaseYear, FNearbyYear, FPercentileEAM, FStock, FAnnualTravel]

type RegionalIntroductionsCube = '[FRegion] ↝ '[FRelativeMarketShare, FIntroductionYear]


introducing :: IntroductionParameters -> FieldRec '[FRegion, FUrbanCode, FUrbanName] -> FieldRec '[FBaseYear, FNearbyYear, FPercentileEAM, FStock, FAnnualTravel] -> FieldRec '[FRelativeMarketShare, FIntroductionYear]
introducing IntroductionParameters{..} key rec =
  let
    base = fBaseYear <: rec
    nearby = fNearbyYear <: rec
    eam = fPercentileEAM <: rec / 100
    region = fRegion <: key
    delay = fromMaybe nan $ region `lookup` delays
    stock = fStock <: rec
    shareIntensification = fromMaybe nan $ region `lookup` shareIntensifications
  in
        fRelativeMarketShare =: shareIntensification * stock
    <+> fIntroductionYear    =: round (base * (1 - clustering) + nearby * clustering + delay * eam)


hasAnnualTravel :: FieldRec '[FRegion, FUrbanCode, FUrbanName] -> FieldRec '[FBaseYear, FNearbyYear, FPercentileEAM, FStock, FAnnualTravel] -> Bool
hasAnnualTravel = const $ not . isNaN . (fAnnualTravel <:)


urbanToRegion :: '[FRegion, FUrbanCode, FUrbanName] ↝ v -> '[FRegion] ↝ v
urbanToRegion =
  rekey
    $ Rekeyer
        (\key -> fRegion =: Region (region (fRegion <: key) ++ " | " ++ urbanCode (fUrbanCode <: key) ++ " | " ++ urbanName (fUrbanName <: key)))
        undefined


introductionYears :: IntroductionParameters -> UrbanCharacteristicsCube -> RegionalIntroductionsCube
introductionYears =
  (. σ hasAnnualTravel)
    . (urbanToRegion .)
    . π
    . introducing
