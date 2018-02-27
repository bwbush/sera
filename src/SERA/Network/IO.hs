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


import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logCritical, logDebug, logInfo, logError, logWarning)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.List (sort)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA (SeraLog, checkDisjoint, checkDuplicates, checkPresent, readConcat, readFractionsConcat)
import SERA.Network.Algorithms
import SERA.Util (extractKey, extractValue)
import SERA.Network.Types (ExistingCube, LinkCube, fFrom, FLocation, fLocation, Network(..), NodeCube, TerritoryCube, fTo, ZoneCube)

import qualified Data.Map.Strict as M (elems)
import qualified Data.Set as S (unions)


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
    checkDuplicates logDebug "existing endpoint" (fLocation <:) $ M.elems records
    return records


readTerritories :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m TerritoryCube
readTerritories = readFractionsConcat "network territories" "territory location"


readZones :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (ZoneCube '[FLocation])
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
