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
-- | Regional and local etworks.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}


module SERA.Network (
-- * Types
  Network(..)
-- * Regional networks
, Path
, AdjacencyMatrix
, ShortestPaths
, pathLength
-- * Input/output
, NetworkFiles(..)
, readNetwork
, readNodes
, readLinks
, readExistings
, readTerritories
, readZones
-- * Quality assurance
, checkNetwork
) where


import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logCritical, logNotice, logInfo, logError, logWarning)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.List (sort)
import Data.Set (Set)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA (SeraLog, checkDisjoint, checkDuplicates, checkPresent, readConcat, readFractionsConcat)
import SERA.Network.Algorithms (AdjacencyMatrix, Path, ShortestPaths, adjacencyMatrix, pathLength, shortestPaths)
import SERA.Types.Cubes (ExistingCube, LinkCube, NodeCube, TerritoryCube, ZoneCube)
import SERA.Types.Fields (fFrom, Location, FLocation, fLocation, fTo)
import SERA.Util (extractKey, extractValue)

import qualified Data.Map.Strict as M (empty, elems)
import qualified Data.Set as S (union, unions)


data Network =
  Network
  {
    nodeCube :: NodeCube
  , linkCube :: LinkCube
  , existingCube :: ExistingCube
  , territoryCube :: TerritoryCube
  , zoneCube :: ZoneCube '[FLocation]
  , adjacencies :: AdjacencyMatrix
  , paths    :: ShortestPaths
  }
    deriving (Eq, Ord, Show)


data NetworkFiles =
  NetworkFiles
  {
    nodeFiles      :: [FilePath]
  , linkFiles      :: [FilePath]
  , existingFiles  :: [FilePath]
  , territoryFile  :: Maybe FilePath
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
    let
      nodes = extractKey (fLocation <:) nodeCube
      links = extractKey (fLocation <:) linkCube
      locations = nodes `S.union` links
    existingCube <- readExistings existingFiles
    territoryCube <- maybe (return M.empty) (readTerritories locations) territoryFile
    zoneCube <- readZones locations zoneFiles
    let
      adjacencies = adjacencyMatrix nodeCube linkCube
      paths = shortestPaths singleLinkPaths maximumPathLength adjacencies
    return Network{..}


readNodes :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m NodeCube
readNodes = readConcat "network nodes" "node location"


readLinks :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m LinkCube
readLinks files =
  do
    records <- readConcat "network links" "link location" files
    checkDuplicates logWarning "link endpoints" (\rec -> sort [fFrom <: rec, fTo <: rec]) $ M.elems records
    return records


readExistings :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m ExistingCube
readExistings files =
  do
    records <- readConcat "existing facilitiess" "existing infrastructure" files
    checkDuplicates logNotice "existing endpoint" (fLocation <:) $ M.elems records
    return records


readTerritories :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => Set Location -> FilePath ->  m TerritoryCube
readTerritories locations file = readFractionsConcat "network territories" "territory location" locations [file]


readZones :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => Set Location -> [FilePath] ->  m (ZoneCube '[FLocation])
readZones = readFractionsConcat "network zones" "zone location"


checkNetwork :: SeraLog m => Network -> m ()
checkNetwork Network{..} =
  do
    logInfo "Checking network . . ."
    let
      nodes = extractKey (fLocation <:) nodeCube
      endpoints =
        S.unions
          [
            extractValue (fFrom <:) linkCube
          , extractValue (fTo <:) linkCube
          ]
    checkPresent
      logError
      "Network links"
      endpoints
      "network nodes"
      nodes
    checkPresent
      logWarning
      "Network nodes"
      nodes
      "network links"
      endpoints
    checkPresent
      logError
      "Existing infrastructures"
      (extractValue (fLocation <:) existingCube)
      "network nodes"
      nodes
    checkPresent
      logError
      "Network territories"
      (extractKey (fLocation <:) territoryCube)
      "network nodes"
      nodes
    checkPresent
      logError
      "Network zones"
      (extractKey (fLocation <:) zoneCube)
      "network nodes"
      nodes
    checkDisjoint
      logCritical
      "Network link has the same location as network node at"
      (extractKey (fLocation <:) linkCube)
      nodes
