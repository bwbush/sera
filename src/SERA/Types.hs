-----------------------------------------------------------------------------
--
-- Module      :  SERA.Types
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Types for modeling.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TupleSections              #-}


module SERA.Types (
-- * Geographic regions
  Region(..)
, FRegion
, fRegion
-- * Calendar year
, Year
, FYear
, fYear
-- * Configuration
, quotedStringTypes
) where


import Control.Arrow (first)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withText)
import Data.Default (Default)
import Data.String.ToString (toString)
import Data.Vinyl.Derived (SField(..))
import GHC.Generics (Generic)


-- | Whether to quote string types in 'Show' and 'Read' instances.
quotedStringTypes :: Bool
quotedStringTypes = False


-- | Data type for geographic regions.
newtype Region = Region {region :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read Region where
  readsPrec
    | quotedStringTypes = (fmap (first Region) .) . readsPrec
    | otherwise         = const $ return . (, []) . Region

instance Show Region where
  show
    | quotedStringTypes = show . region
    | otherwise         = region

instance FromJSON Region where
  parseJSON = withText "SERA.Types.Region" $ return . Region . toString

instance ToJSON Region where
  toJSON = toJSON . region


-- | Field type for geographic regions.
type FRegion = '("Region", Region)


-- | Field label for geographic regions.
fRegion :: SField FRegion
fRegion = SField


-- | Data type for calendar years.
type Year = Int


-- | Field type for calendar years.
type FYear = '("Year", Year)


-- | Field label for calendar years.
fYear :: SField FYear
fYear = SField
