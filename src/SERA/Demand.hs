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


{-# LANGUAGE FlexibleContexts    #-}


module SERA.Demand (
-- * Types
  DemandCube
-- * Input/output
, readDemands
-- * Testing
, checkDemands
) where


import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logDebug, logInfo, logWarning)
import Data.Daft.Vinyl.FieldCube (τ)
import Data.Daft.Vinyl.FieldRec ((<:), (<+>))
import Data.Daft.Vinyl.FieldRec.IO (readFieldRecFile)
import Data.String (IsString)
import SERA (SeraLog, checkPresent)
import SERA.Network.Types (NodeCube, fLocation)
import SERA.Types.Cubes (DemandCube)
import SERA.Types.Fields (fFuelConsumption, fNonFuelConsumption)
import SERA.Types.Records (DemandRec)
import SERA.Util (combineRecs, extractKey)

import qualified Data.Map.Strict as M (fromListWith)


-- | Read demand data into a cube.
readDemands :: (IsString e, MonadError e m, MonadIO m, SeraLog m)
            => Bool                                    -- ^ Whether to remove records with zero demand.
            -> [FilePath]                              -- ^ The files.
            -> m DemandCube                            -- ^ Action for reading the files into a demand cube.
readDemands removeZeroDemand files =
  (
    M.fromListWith (\rec rec' -> combineRecs fFuelConsumption (+) rec rec' <+> combineRecs fNonFuelConsumption (+) rec rec')
      . fmap (\rec -> (τ (rec :: DemandRec), τ rec))
      . filter (\rec -> not removeZeroDemand || fFuelConsumption <: rec /= 0 || fNonFuelConsumption <: rec /= 0)
      . mconcat
  )
  <$> sequence
  [
    do
      logInfo $ "Reading demands from \"" ++ file ++ "\" . . ."
      readFieldRecFile file
  |
    file <- files
  ]


-- | Check the validity of the demands.
checkDemands :: SeraLog m
             => NodeCube   -- ^ The network nodes.
             -> DemandCube -- ^ The demands.
             -> m ()       -- ^ Action for checking the demands.
checkDemands nodeCube demandCube =
  do
    let
      networkNodes = extractKey (fLocation <:) nodeCube
      demandNodes  = extractKey (fLocation <:) demandCube
    logInfo "Checking demands . . ."
    checkPresent logWarning "Demand locations" demandNodes  "network nodes"   networkNodes
    checkPresent logDebug   "Network nodes"    networkNodes "demand location" demandNodes
