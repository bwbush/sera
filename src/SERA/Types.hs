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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Types (
-- * Geographic regions
  Region(..)
, FRegion
, fRegion
-- * Calendar year
, Year
, FYear
, fYear
, pushYear
, minimumYear
-- * Urban area
, UrbanCode(..)
, FUrbanCode
, fUrbanCode
, UrbanName(..)
, FUrbanName
, fUrbanName
-- * Configuration
, quotedStringTypes
) where


import Control.Arrow (first)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withText)
import Data.Daft.Vinyl.FieldCube (τ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Default (Default)
import Data.String.ToString (toString)
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Vinyl.Lens (type (∈))
import GHC.Generics (Generic)
import SERA.Types.TH (makeField, makeStringField, quotedStringTypes)


-- | Data type for geographic regions.
$(makeStringField "Region" "Region" "Region")


-- | Data type for calendar years.
type Year = Int


-- | Field type for calendar years.
type FYear = '("Year", Year)


-- | Field label for calendar years.
fYear :: SField FYear
fYear = SField


-- | Transfer a year from the key to the value.
pushYear :: (FYear ∈ ks)
         => FieldRec ks       -- ^ The key.
         -> v                 -- ^ The value.
         -> FieldRec '[FYear] -- ^ The years.
pushYear key _ = τ key


-- | Find the minimum year.
minimumYear :: (FYear ∈ vs)
            => k                 -- ^ The key.
            -> [FieldRec vs]     -- ^ The values.
            -> FieldRec '[FYear] -- ^ The minimum year.
minimumYear _ recs = fYear =: minimum ((fYear <:) <$> recs)


-- | Data type for urban areas codes.
newtype UrbanCode = UrbanCode {urbanCode :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read UrbanCode where
  readsPrec
    | quotedStringTypes = (fmap (first UrbanCode) .) . readsPrec
    | otherwise         = const $ return . (, []) . UrbanCode

instance Show UrbanCode where
  show
    | quotedStringTypes = show . urbanCode
    | otherwise         = urbanCode

instance FromJSON UrbanCode where
  parseJSON = withText "SERA.Types.UrbanCode" $ return . UrbanCode . toString

instance ToJSON UrbanCode where
  toJSON = toJSON . urbanCode


-- | Field type for urban area codes.
type FUrbanCode = '("Census Urban Area Code", UrbanCode)


-- | Field label for urban area codes.
fUrbanCode :: SField FUrbanCode
fUrbanCode = SField


-- | Data type for urban area names.
newtype UrbanName = UrbanName {urbanName :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read UrbanName where
  readsPrec
    | quotedStringTypes = (fmap (first UrbanName) .) . readsPrec
    | otherwise         = const $ return . (, []) . UrbanName

instance Show UrbanName where
  show
    | quotedStringTypes = show . urbanName
    | otherwise         = urbanName

instance FromJSON UrbanName where
  parseJSON = withText "SERA.Types.UrbanName" $ return . UrbanName . toString

instance ToJSON UrbanName where
  toJSON = toJSON . urbanName


-- | Field type for urban area names.
type FUrbanName = '("Census Urban Area Name", UrbanName)


-- | Field label for urban area names.
fUrbanName :: SField FUrbanName
fUrbanName = SField
