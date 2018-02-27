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
) where


import Control.Monad (guard)
import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logDebug, logInfo, logError, logWarning)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Vinyl.FieldCube (fromRecords)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Daft.Vinyl.FieldRec.IO (readFieldRecFile)
import Data.Function.MapReduce (groupReduceByKey)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA (SeraLog)
import SERA.Network.Algorithms
import SERA.Network.Types (ExistingCube, LinkCube, fFrom, FLocation, fLocation, Network(..), NodeCube, TerritoryCube, fTerritory, fTo, ZoneCube, fZone)
import SERA.Types (fFraction)


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


checkDuplicates :: (SeraLog m, Ord b, Show b) => (String -> m ()) -> String -> (a -> b) -> [a] -> m ()
checkDuplicates logMessage label field records =
  sequence_
    [
      logMessage $ "Duplicate " ++ label ++ " \"" ++ show location ++ "\"."
    |
      location <-
        catMaybes
          $ groupReduceByKey
            field
            (flip ((>>) . guard . (> 1) . length) . return)
            records
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
    checkDuplicates logError "existing infrastructure" (fLocation <:) records
    checkDuplicates logDebug "existing endpoint" (fLocation <:) records
    return $ fromRecords records


epsilon :: Double
epsilon = 1e-5


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
      key rec = (fTerritory <: rec, fLocation <: rec)
    checkDuplicates logError "territory location" key records
    checkFractions "territories" key records
    return $ fromRecords records


readZones :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [FilePath] ->  m (ZoneCube '[FLocation])
readZones files =
  do
    records <- readConcat "zones" files
    let
      key rec = (fZone <: rec, fLocation <: rec)
    checkDuplicates logError "zone location" key records
    checkFractions "zones" key records
    return $ fromRecords records
