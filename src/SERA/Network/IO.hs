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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RecordWildCards   #-}


module SERA.Network.IO (
  readNetwork
, readNodes
, readLinks
, readExistings
, readTerritories
, readZones
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Daft.Vinyl.FieldCube ((⋈), κ, ω)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set, toList)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import SERA.Network.Types (FLocation, FZone, ExistingCube, LinkCube, Network(..), NodeCube, TerritoryCube, ZoneCube)
import SERA.Types (FRegion, FYear)


readNetwork :: (IsString e, MonadError e m, MonadIO m) => [FilePath] -> [FilePath] -> [FilePath] -> [FilePath] -> [FilePath] ->  m Network
readNetwork nodeFiles linkFiles existingFiles territoryFiles zoneFiles =
  do
    nodeCube <- readNodes nodeFiles
    linkCube <- readLinks linkFiles
    existingCube <- readExistings existingFiles
    territoryCube <- readTerritories territoryFiles
    zoneCube <- readZones zoneFiles
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
