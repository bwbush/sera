-----------------------------------------------------------------------------
--
-- Module      :  $Header$
-- Copyright   :  (c) 2018 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Energy demand.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}


module SERA.Demand (
-- * Types
  DemandCube
-- * Input/output
, readDemands
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Function (on)
import Data.Daft.Vinyl.FieldCube (τ)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Daft.Vinyl.FieldRec.IO (readFieldRecFile)
import Data.String (IsString)
import SERA.Types.Cubes (DemandCube)
import SERA.Types.Fields (fFuelConsumption, fNonFuelConsumption)
import SERA.Types.Records (DemandRec)

import qualified Data.Map.Strict as M (fromListWith)


-- FIXME: Move to 'daft' package.
combineRecs field operation x y = field =: on operation (field <:) x y


-- | Read demand data into a cube.
readDemands :: (IsString e, MonadError e m, MonadIO m)
            => Bool                                    -- ^ Whether to remove records with zero demand.
            -> [FilePath]                              -- ^ The files.
            -> m DemandCube                            -- ^ Action for reading the files into a demand cube.
readDemands removeZeroDemand =
  fmap
    (
      M.fromListWith (\rec rec' -> combineRecs fFuelConsumption (+) rec rec' <+> combineRecs fNonFuelConsumption (+) rec rec')
        . fmap (\rec -> (τ (rec :: DemandRec), τ rec))
        . filter (\rec -> not removeZeroDemand || fFuelConsumption <: rec /= 0 || fNonFuelConsumption <: rec /= 0)
        . mconcat
    )
    . mapM readFieldRecFile
