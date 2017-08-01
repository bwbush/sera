{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


module SERA.Network.Algorithms
-- FIXME
where


import Data.Daft.Vinyl.FieldCube (toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Default.Util (inf)
import Data.Map.Strict (Map)
import Data.PQueue.Min (MinQueue)
import SERA.Network.Types

import qualified Data.Map.Strict as M ((!), empty, fromList, fromListWith, insert, keys, map, singleton, toList, union, unions)
import qualified Data.PQueue.Min as Q (deleteFindMin, fromList, insert, null, union)


type Adjacencies = Map Location (Map Location (Location, Double))


type ShortestPaths = Map (Location, Location) Path


adjacencies :: Network -> Adjacencies
adjacencies Network{..} =
  M.fromListWith
    M.union
    $ concat
    [
      [
        (fFrom <: rec, M.singleton (fTo   <: rec) (fLocation <: rec, fLength <: rec))
      , (fTo   <: rec, M.singleton (fFrom <: rec) (fLocation <: rec, fLength <: rec))
      ]
    |
      rec <- toKnownRecords linkCube
    ]
    ++
    [
      (fLocation <: rec, M.empty)
    |
      rec <- toKnownRecords nodeCube
    ]


shortestPaths :: Network -> ShortestPaths
shortestPaths network =
  let
    as = adjacencies network
  in
    M.unions
      [
        shortestPathTree as root
      |
        root <- M.keys as
      ]
      

shortestPathTree :: Adjacencies -> Location -> ShortestPaths
shortestPathTree as root =
  let
    tree =
      shortestPathTree' 
        as
        M.empty
        (M.insert root 0 $ M.map (const inf) as)
        (Q.insert (0, root) $ Q.fromList $ map (inf, ) $ M.keys as)
  in
    walkTree as -- handle as fold


walkTree :: Adjacencies -> ShortestPaths -> Set Location -> ShortestPaths
walkTree as paths fringe =
  undefined


shortestPathTree' :: Adjacencies -> Map Location Location -> Map Location Double -> MinQueue (Double, Location) -> Map Location Location
shortestPathTree' as previous distances unseen'' =
  let
    ((distance, vertex), unseen) = Q.deleteFindMin unseen''
    (updatePrevious, updateDistances, updateUnseen) =
      unzip3
        [
          (
            (vertex, vertex')
          , (vertex', distance')
          , (distance', vertex')
          )
        |
          (vertex', (_, length')) <- M.toList $ as M.! vertex
        , let distance' = distance + length'
        , distance' < distances M.! vertex'
        ]
    previous' = M.fromList updatePrevious `M.union` previous
    distances' = M.fromList updateDistances `M.union` distances
    unseen' = Q.fromList updateUnseen `Q.union` unseen
  in
    if Q.null unseen'
      then previous'
      else shortestPathTree' as previous' distances' unseen'
