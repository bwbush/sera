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
, readPeriods
-- * Testing
, checkDemands
) where


import Control.Monad (when)
import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logError, logNotice, logInfo, logWarning)
import Data.Daft.DataCube (knownSize)
import Data.Daft.Vinyl.FieldCube (τ, toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((<:), (<+>))
import Data.Daft.Vinyl.FieldRec.IO (readFieldRecFile)
import Data.String (IsString)
import SERA (SeraLog, checkPresent, readConcat)
import SERA.Types.Cubes (DemandCube, NodeCube, PeriodCube)
import SERA.Types.Fields (fDuration, fFuelConsumption, fLocation, fNonFuelConsumption)
import SERA.Types.Records (DemandRec)
import SERA.Util (combineRecs, extractKey)

import qualified Data.Map.Strict as M (fromListWith)


-- | Read demand data into a cube.
readDemands :: (IsString e, MonadError e m, MonadIO m, SeraLog m)
            => Bool                                    -- ^ Whether to remove records with zero demand.
            -> [FilePath]                              -- ^ The files.
            -> m DemandCube                            -- ^ Action for reading the files into a demand cube.
readDemands removeZeroDemand files =
  do
    records <-
      mconcat
        <$> sequence
        [
          do
            logInfo $ "Reading demands from \"" ++ file ++ "\" . . ."
            records' <- readFieldRecFile file
            logInfo $ " . . . " ++ show (length records') ++ " records read."
            return records'
        |
          file <- files
        ]
    let
      cube =
        M.fromListWith (\rec rec' -> combineRecs fFuelConsumption (+) rec rec' <+> combineRecs fNonFuelConsumption (+) rec rec')
          . fmap (\rec -> (τ (rec :: DemandRec), τ rec))
          $ filter (\rec -> not removeZeroDemand || fFuelConsumption <: rec /= 0 || fNonFuelConsumption <: rec /= 0)
            records
    logInfo $ "Total of " ++ show (knownSize cube) ++ " records for demands."
    return cube


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
    checkPresent logNotice   "Network nodes"    networkNodes "demand location" demandNodes


readPeriods :: (IsString e, MonadError e m, MonadIO m, SeraLog m)
            => [FilePath]                              -- ^ The files.
            -> m PeriodCube
readPeriods files =
  do
    result <- readConcat "time periods" "time period" files
    let
      total = sum $ (fDuration <:) <$> toKnownRecords result :: Double
    when (total /= 1)
      . logError $ "Duration does not sum to one year: " ++ show total ++ "."
    return result
