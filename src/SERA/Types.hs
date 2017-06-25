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
-- * Fractions
, FFraction
, fFraction
-- * Configuration
, quotedStringTypes
) where


import Data.Daft.Vinyl.FieldCube (τ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import SERA.Types.TH (makeField, makeStringField, quotedStringTypes)


-- | Data type for geographic regions.
$(makeStringField "Region" "Region")


-- | Data type for calendar years.
type Year = Int


-- | Field type for calendar years.
-- | Field label for calendar years.
$(makeField "Year" "Year" ''Year)


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
-- | Field type for urban area codes.
-- | Field label for urban area codes.
$(makeStringField "UrbanCode" "Census Urban Area Code")


-- | Data type for urban area names.
-- | Field type for urban area names.
$(makeStringField "UrbanName" "Census Urban Area Name")


$(makeField "Fraction" "Fraction" ''Double)
