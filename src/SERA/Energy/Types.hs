-----------------------------------------------------------------------------
--
-- Module      :  SERA.Energy.Types
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Types for energy computations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeOperators             #-}


module SERA.Energy.Types (
-- * Data cubes
  EnergyPriceCube
, FeedstockUsageCube
, CarbonCreditCube
, UtilizationCube
-- * Field types and labels
, HydrogenSource(..)
, FHydrogenSource
, fHydrogenSource
, FeedstockType(..)
, FFeedstockType
, fFeedstockType
, FFeedstockUsage
, fFeedstockUsage
, FUtilization
, fUtilization
, FNonRenewablePrice
, fNonRenewablePrice
, FRenewablePrice
, fRenewablePrice
, FNonRenewableCredit
, fNonRenewableCredit
, FRenewableCredit
, fRenewableCredit
) where


import Control.Arrow (first)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), withText)
import Data.Daft.Vinyl.FieldCube -- (type (↝), π, σ)
import Data.String.ToString (toString)
import Data.Vinyl.Derived (SField(..))
import SERA.Service ()
import SERA.Types


type EnergyPriceCube = '[FYear, FFeedstockType] ↝ '[FNonRenewablePrice, FRenewablePrice]


type FeedstockUsageCube = '[FHydrogenSource, FFeedstockType] ↝ '[FFeedstockUsage]


type CarbonCreditCube = '[FHydrogenSource] ↝ '[FNonRenewableCredit, FRenewableCredit]


type UtilizationCube = '[FYear, FRegion] ↝ '[FUtilization]


newtype HydrogenSource = HydrogenSource {hydrogenSource :: String}
  deriving (Eq, Ord)

instance Read HydrogenSource where
  readsPrec
    | quotedStringTypes = (fmap (first HydrogenSource) .) . readsPrec
    | otherwise         = const $ return . (, []) . HydrogenSource

instance Show HydrogenSource where
  show
    | quotedStringTypes = show . hydrogenSource
    | otherwise         = hydrogenSource

instance FromJSON HydrogenSource where
  parseJSON = withText "HydrogenSource" $ return . HydrogenSource . toString

instance ToJSON HydrogenSource where
  toJSON = toJSON . hydrogenSource


type FHydrogenSource = '("Hydrogen Source", HydrogenSource)


fHydrogenSource :: SField FHydrogenSource
fHydrogenSource = SField


newtype FeedstockType = FeedstockType {feedstockType :: String}
  deriving (Eq, Ord)

instance Read FeedstockType where
  readsPrec
    | quotedStringTypes = (fmap (first FeedstockType) .) . readsPrec
    | otherwise         = const $ return . (, []) . FeedstockType

instance Show FeedstockType where
  show
    | quotedStringTypes = show . feedstockType
    | otherwise         = feedstockType

instance FromJSON FeedstockType where
  parseJSON = withText "FeedstockType" $ return . FeedstockType . toString

instance ToJSON FeedstockType where
  toJSON = toJSON . feedstockType


type FFeedstockType = '("Feedstock", FeedstockType)


fFeedstockType :: SField FFeedstockType
fFeedstockType = SField


type FFeedstockUsage = '("Feedstock Usage [/kg]", Double)


fFeedstockUsage :: SField FFeedstockUsage
fFeedstockUsage = SField


type FNonRenewablePrice = '("Non-Renewable Price [$]", Double)


fNonRenewablePrice :: SField FNonRenewablePrice
fNonRenewablePrice = SField


type FRenewablePrice = '("Renewable Price [$]", Double)


fRenewablePrice :: SField FRenewablePrice
fRenewablePrice = SField


type FNonRenewableCredit = '("Carbon Credit (Non-Renewable) [$/kg]", Double)


fNonRenewableCredit :: SField FNonRenewableCredit
fNonRenewableCredit = SField


type FRenewableCredit = '("Carbon Credit (Renewable) [$/kg]", Double)


fRenewableCredit :: SField FRenewableCredit
fRenewableCredit = SField


type FUtilization = '("Utilization [kg/kg]", Double)


fUtilization :: SField FUtilization
fUtilization = SField
