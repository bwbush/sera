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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Types (
  pushYear
, minimumYear
) where


import Data.Daft.Vinyl.FieldCube (τ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import SERA.Types.Fields (FYear, fYear)


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
