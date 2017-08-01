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


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA.Network.Algorithms
import SERA.Network.Types (ExistingCube, LinkCube, FLocation, Network(..), NodeCube, TerritoryCube, ZoneCube)


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


readNetwork :: (IsString e, MonadError e m, MonadIO m) => Double -> NetworkFiles ->  m Network
readNetwork maximumPathLength NetworkFiles{..} =
  do
    nodeCube <- readNodes nodeFiles
    linkCube <- readLinks linkFiles
    existingCube <- readExistings existingFiles
    territoryCube <- readTerritories territoryFiles
    zoneCube <- readZones zoneFiles
    let
      adjacencies = adjacencyMatrix nodeCube linkCube
      paths = shortestPaths maximumPathLength adjacencies
    return Network{..}


readNodes :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m NodeCube
readNodes = (mconcat <$>) . mapM readFieldCubeFile


readLinks :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m LinkCube
readLinks = (mconcat <$>) . mapM readFieldCubeFile


readExistings :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m ExistingCube
readExistings = (mconcat <$>) . mapM readFieldCubeFile


readTerritories :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m TerritoryCube
readTerritories = (mconcat <$>) . mapM readFieldCubeFile


readZones :: (IsString e, MonadError e m, MonadIO m) => [FilePath] ->  m (ZoneCube '[FLocation])
readZones = (mconcat <$>) . mapM readFieldCubeFile
