-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.Logistic
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for logistic scenario generation.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Service.Intraurban (
-- * Configuration
  ConfigIntraurban(..)
-- * Computation
, intraurbanMain
) where



import Control.Arrow ((&&&))
import Control.Monad (guard)
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.Vinyl.FieldCube (type (↝), ω, toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Daft.Vinyl.FieldRec.IO (readFieldRecFile, writeFieldRecFile)
import Data.List (partition, sort)
import Data.List.Util (sortedGroupsOn)
import Data.Map (Map)
import Data.Maybe (isJust, mapMaybe)
import Data.PQueue.Prio.Max (MaxPQueue)
import Data.Set (Set)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import Data.Void (Void)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import SERA (verboseReadFieldCubeSource, verboseWriteFieldCubeSource)
import SERA.Infrastructure.Types (Position(Position), FPosition, fPosition)
import SERA.Network.Types (FLength, fLength, Location(..), FLocation, fLocation, FFrom, fFrom, FTo, fTo, FX, fX, FY, fY)
import SERA.Refueling.Types (FNewCapacity, fNewCapacity)
import SERA.Service ()
import SERA.Types (Cluster(cluster), FCluster, fCluster, Geometry(Geometry), FGeometry, fGeometry, Year, FYear, fYear)

import qualified Data.Map.Strict as M ((!), elems, empty, fromList, insert, size, union)
import qualified Data.PQueue.Prio.Max as Q (empty, deleteFindMax, filter, fromList, null, size, union, unions)
import qualified Data.Set as S (delete, elems, empty, fromList, insert, size, union)


-- | Configuration for logistic curve computations.
data ConfigIntraurban =
  ConfigIntraurban
  {
    pipelineThreshold     :: Double          -- units: kg/day/mile
  , measureX              :: Double
  , measureY              :: Double
  , stationLocationFile :: FilePath -- ^ Source for station details.
  , stationLocationOutputFile :: FilePath
  , pipelineFile        :: FilePath
  , geometryFile        :: FilePath
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigIntraurban

instance ToJSON ConfigIntraurban


intraurbanMain :: (IsString e, MonadError e m, MonadIO m)
                  => ConfigIntraurban -- ^ Configuration data.
                  -> m ()           -- ^ Action to compute the logistic scenario.
intraurbanMain ConfigIntraurban{..} =
  do
    stationLocations <- readFieldRecFile stationLocationFile
    let
      score station station' =
        let
          x  = fX <: station
          y  = fY <: station
          x' = fX <: station'
          y' = fY <: station'
          two = 2 :: Int
          distance = sqrt $ (measureX * (x - x'))^two + (measureY * (y - y'))^two
          score' = fNewCapacity <: station / distance
        in
          do
            guard $ score' >= pipelineThreshold
            return (score', distance)
      (stations, pipelines) = buildPipelines score stationLocations
    writeFieldRecFile stationLocationOutputFile
      [
            station
        <+> fGeometry =: Geometry ("POINT( " ++ show(fX <: station) ++ " " ++ show(fY <: station) ++ " )")
      |
        station <- stations
      ]
    writeFieldRecFile pipelineFile
      [
            pipeline
        <+> fGeometry =: Geometry ("LINESTRING( " ++ positions M.! station ++ " , " ++ positions M.! station' ++ " )")
      |
        let positions = M.fromList [(fLocation <: station, show (fX <: station) ++ " " ++ show (fY <: station)) | station <- stations]
        , pipeline <- pipelines
        , let station  = fFrom <: pipeline
        , let station' = fTo   <: pipeline
      ]
    writeFieldRecFile geometryFile
      $ concat
      $ [
              fLocation =: fLocation <: station
          <+> fPosition =: Position "center"
          <+> fX        =: fX        <: station
          <+> fY        =: fY        <: station
        |
          station <- stations
        ]
      :
        [
          [
            fLocation =: fLocation <: pipeline <+> fPosition =: Position "from" <+> positions M.! station
          , fLocation =: fLocation <: pipeline <+> fPosition =: Position "to"   <+> positions M.! station'
          ]
        |
          let positions = M.fromList [(fLocation <: station, fX =: fX <: station <+> fY =: fY <: station) | station <- stations]
        , pipeline <- pipelines
        , let station  = fFrom <: pipeline
        , let station' = fTo   <: pipeline
        ]

type Score = (FX ∈ rs, FY ∈ rs, FNewCapacity ∈ rs) => FieldRec rs -> FieldRec rs -> Maybe (Double, Double)

type Station = FieldRec '[FLocation, FYear, FX, FY, FNewCapacity, FCluster]

type Pipeline' = FieldRec '[FLocation, FYear, FFrom, FTo, FLength, FCluster]
type Pipeline = FieldRec '[FYear, FFrom, FTo, FLength, FCluster]

type Clusters = Map Location Station

type Unclustereds = Set Station

type Scores = MaxPQueue Double (Pipeline, Station)


buildPipelines :: Score -> [Station] -> ([Station], [Pipeline'])
buildPipelines score stationLocations =
  let
    years = sortedGroupsOn (fYear <:) id stationLocations
    (clusters, unclustereds, _, pipelines) = foldl (addPipelines score) (M.empty, S.empty, Q.empty, []) years
  in
    (
      sort $ M.elems clusters ++ S.elems unclustereds
    , zipWith (\i p -> fLocation =: Location ("Pipeline #" ++ show i) <+> p) [1..] $ sort $ reverse pipelines
    )


addPipelines :: Score -> (Clusters, Unclustereds, Scores, [Pipeline]) -> (Year, [Station]) -> (Clusters, Unclustereds, Scores, [Pipeline])
addPipelines score (clusters, unclustereds, scores, pipelines) (year, stations) =
  let
    makePipeline station station' =
      do
        (score', distance') <- score station station'
        return
          (
            score'
          , (
                  fYear    =: year
              <+> fFrom    =: fLocation <: station'
              <+> fTo      =: fLocation <: station
              <+> fLength  =: distance'
              <+> fCluster =: fCluster  <: station'
            , station
            )
          )
    (seeds, nonseeds) = partition (isJust . cluster . (fCluster <:)) stations
    clusters' = clusters `M.union` M.fromList (((fLocation <:) &&& id) <$> seeds)
    unclustereds' = unclustereds `S.union` S.fromList nonseeds
    scores' =
      Q.unions
        $ scores
        : [
            Q.fromList
              $ mapMaybe (makePipeline station)
              $ M.elems clusters'
          |
            station <- nonseeds
          ]
  in
    addPipeline makePipeline (clusters', unclustereds', scores', pipelines)


addPipeline :: (Station -> Station -> Maybe (Double, (Pipeline, Station))) -> (Clusters, Unclustereds, Scores, [Pipeline]) -> (Clusters, Unclustereds, Scores, [Pipeline])
addPipeline makePipeline (clusters, unclustereds, scores, pipelines)
  | Q.null scores = (clusters, unclustereds, scores, pipelines)
  | otherwise =
      let
        ((_, (pipeline, station)), scores') = Q.deleteFindMax scores
        station' =
              fLocation    =: fLocation    <: station
          <+> fYear        =: fYear        <: station
          <+> fX           =: fX           <: station
          <+> fY           =: fY           <: station
          <+> fNewCapacity =: fNewCapacity <: station
          <+> fCluster     =: fCluster     <: pipeline     
        unclustereds' = S.delete station unclustereds
      in
        addPipeline makePipeline
          (
            M.insert (fLocation <: station) station' clusters
          , unclustereds'
          , Q.filter ((/= station) . snd) scores'
              `Q.union` Q.fromList (mapMaybe (`makePipeline` station') $ S.elems unclustereds')
          , pipeline : pipelines
          )
