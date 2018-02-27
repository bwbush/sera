-----------------------------------------------------------------------------
--
-- Module      :  SERA.Energy.Prices
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Energy prices.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Network.IO (
  NetworkFiles(..)
, readNetwork
, readNodes
, readLinks
, readExistings
, readTerritories
, readZones
, checkNetwork
) where


import Control.Applicative (liftA2)
import Control.Monad (guard)
import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logCritical, logDebug, logInfo, logError, logWarning)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Vinyl.FieldCube (fromRecords)
import Data.Daft.Vinyl.FieldRec (Labeled(..), (<:))
import Data.Daft.Vinyl.FieldRec.IO (ReadFieldRec, readFieldRecFile)
import Data.Function.MapReduce (groupReduceByKey)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import GHC.Generics (Generic)
import SERA (SeraLog, checkDisjoint, checkDuplicates, checkPresent)
import SERA.Network.Algorithms
import SERA.Util (extractKey, extractValue)
import SERA.Network.Types (ExistingCube, LinkCube, fFrom, fInfrastructure, FLocation, fLocation, Network(..), NodeCube, TerritoryCube, fTerritory, fTo, ZoneCube, fZone)
import SERA.Types (FFraction, fFraction)

import qualified Data.Set as S (union)


data NetworkFiles =
  NetworkFiles
  {
    nodeFiles      :: [FilePath]
  , linkFiles      :: [FilePath]
  , existingFiles  :: [FilePath]
  , territoryFiles :: [FilePath]
  , zoneFiles      :: [FilePath]
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON NetworkFiles

instance ToJSON NetworkFiles


readNetwork :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => Bool -> Double -> NetworkFiles ->  m Network
readNetwork singleLinkPaths maximumPathLength NetworkFiles{..} =
  do
    nodeCube <- readNodes nodeFiles
    linkCube <- readLinks linkFiles
    existingCube <- readExistings existingFiles
    territoryCube <- readTerritories territoryFiles
    zoneCube <- readZones zoneFiles
    let
      adjacencies = adjacencyMatrix nodeCube linkCube
      paths = shortestPaths singleLinkPaths maximumPathLength adjacencies
    return Network{..}


readConcat :: (IsString e, MonadError e m, MonadIO m, SeraLog m, Labeled (FieldRec rs), ReadFieldRec rs) => String -> [FilePath] -> m [FieldRec rs]
readConcat message files =
  mconcat
    <$> sequence
    [
      do
        logInfo $ "Reading network " ++ message ++ " from \"" ++ file ++ "\" . . ."
        readFieldRecFile file
    |
      file <- files
    ]


readNodes :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m NodeCube
readNodes files =
  do
    records <- readConcat "nodes" files
    checkDuplicates logError "node location" (fLocation <:) records
    return $ fromRecords records


readLinks :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m LinkCube
readLinks files =
  do
    records <- readConcat "links" files
    checkDuplicates logError "link location" (fLocation <:) records
    checkDuplicates logWarning "link endpoints" (\rec -> sort [fFrom <: rec, fTo <: rec]) records
    return $ fromRecords records


readExistings :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m ExistingCube
readExistings files =
  do
    records <- readConcat "existing facilitiess" files
    checkDuplicates logError "existing infrastructure" (fInfrastructure <:) records
    checkDuplicates logDebug "existing endpoint" (fLocation <:) records
    return $ fromRecords records


epsilon :: Double
epsilon = 1e-5


checkFractions :: (Ord a, Show a, FFraction ∈ rs, SeraLog m) => String -> (FieldRec rs -> a) -> [FieldRec rs] -> m () 
checkFractions label field records =
  sequence_
    [
      logError $ "Fractions do not sum to one for " ++ label ++ " \"" ++ show location ++ "\"."
    |
      location <-
        catMaybes
          $ groupReduceByKey
            field
            (flip ((>>) . guard . (>= epsilon) . abs . ((-) 1) . sum . fmap (fFraction <:)) . return)
            records
    ]


readTerritories :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m TerritoryCube
readTerritories files =
  do
    records <- readConcat "territories" files
    let
      key = liftA2 (,) (fTerritory <:) (fLocation <:)
    checkDuplicates logError "territory location" key records
    checkFractions "territory" key records
    return $ fromRecords records


readZones :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (ZoneCube '[FLocation])
readZones files =
  do
    records <- readConcat "zones" files
    let
      key = liftA2 (,) (fZone <:) (fLocation <:)
    checkDuplicates logError "zone location" key records
    checkFractions "zone" key records
    return $ fromRecords records


checkNetwork :: SeraLog m => Network -> m ()
checkNetwork Network{..} =
  do
    logInfo "Checking network . . ."
    let
      nodes = extractKey (fLocation <:) nodeCube
    checkPresent
      logError
      "Network link"
      ((extractValue (fFrom <:) linkCube) `S.union` (extractValue (fTo <:) linkCube))
      "network nodes"
      nodes
    checkPresent
      logError
      "Existing infrastructure"
      (extractValue (fLocation <:) existingCube)
      "network nodes"
      nodes
    checkPresent
      logError
      "Network territory"
      (extractKey (fLocation <:) territoryCube)
      "network nodes"
      nodes
    checkPresent
      logError
      "Network zone"
      (extractKey (fLocation <:) zoneCube)
      "network nodes"
      nodes
    checkDisjoint
      logCritical
      "Network link has the same location as network node at"
      (extractKey (fLocation <:) linkCube)
      nodes
