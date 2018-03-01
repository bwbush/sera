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
import SERA.Types.Cubes
import SERA.Types.Fields

import qualified Data.Map.Strict as M ((!), empty, fromList, fromListWith, insert, lookup, keys, map, singleton, toList, union, unions)
import qualified Data.PQueue.Min as Q (deleteFindMin, fromList, insert, null, union)


data Path =
    GenericPath -- FIXME: Enforce semantics.
    {
      sourceId        :: Location
    , linkIds         :: [(Location, Double)]
    , sinkId          :: Location
    }
{-
  | TransmissionPath -- FIXME: Enforce semantics.
    {
      sourceId        :: Location
    , transmissionIds :: [(Location, Double)]
    , gateId          :: Location
    }
  | DeliveryPath -- FIXME: Enforce semantics.
    {
      gateId          :: Location
    , deliveryIds     :: [(Location, Double)]
    , sinkId          :: Location
    }
  | FullPath -- FIXME: Enforce semantics.
    {
      sourceId        :: Location
    , transmissionIds :: [(Location, Double)]
    , gateId          :: Location
    , deliveryIds     :: [(Location, Double)]
    , sinkId          :: Location
    }
-}
    deriving (Eq, Ord, Read, Show)


pathLength :: Path -> Double
pathLength = sum . fmap snd . linkIds


type AdjacencyMatrix = Map Location (Map Location (Location, Double))


type ShortestPaths = Map (Location, Location) Path


adjacencyMatrix :: NodeCube -> LinkCube -> AdjacencyMatrix
adjacencyMatrix nodeCube linkCube =
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


shortestPaths :: Bool -> Double -> AdjacencyMatrix -> ShortestPaths
shortestPaths False maximumPathLength adjacencies =
  M.unions
    [
      shortestPathTree maximumPathLength adjacencies root
    |
      root <- M.keys adjacencies
    ]
shortestPaths True maximumPathLength adjacencies =
  M.fromList
    [
      ((src, dst), GenericPath src [(link, distance)] dst)
    |
      (src, dsts) <- M.toList adjacencies
    , (dst, (link, distance)) <- M.toList dsts
    , distance < maximumPathLength
    ]

shortestPathTree :: Double -> AdjacencyMatrix -> Location -> ShortestPaths
shortestPathTree maximumPathLength adjacencies root =
  let
    tree =
      shortestPathTree' 
        adjacencies
        M.empty
        (M.insert root 0 $ M.map (const inf) adjacencies)
        (Q.insert (0, root) $ Q.fromList $ map (inf, ) $ M.keys adjacencies)
  in
    M.fromList
      [
        ((sourceId, sinkId), path)
      |
        let sourceId = root
      , let linkIds = []
      , sinkId <- M.keys tree
      , let path = walkTree adjacencies tree sinkId $ GenericPath{..}
      , pathLength path <= maximumPathLength
      ]


walkTree :: AdjacencyMatrix -> Map Location Location -> Location -> Path -> Path
walkTree adjacencies tree vertex' path@GenericPath{..} =
  case vertex' `M.lookup` tree of
    Nothing     -> path
    Just vertex -> walkTree adjacencies tree vertex $ GenericPath vertex ((adjacencies M.! vertex) M.! vertex' : linkIds) sinkId
  

shortestPathTree' :: AdjacencyMatrix -> Map Location Location -> Map Location Double -> MinQueue (Double, Location) -> Map Location Location
shortestPathTree' adjacencies previous distances unseen'' =
  let
    ((distance, vertex), unseen) = Q.deleteFindMin unseen''
    (updatePrevious, updateDistances, updateUnseen) =
      unzip3
        [
          (
            (vertex', vertex)
          , (vertex', distance')
          , (distance', vertex')
          )
        |
          (vertex', (_, length')) <- M.toList $ adjacencies M.! vertex
        , let distance' = distance + length'
        , distance' < distances M.! vertex'
        ]
    previous' = M.fromList updatePrevious `M.union` previous
    distances' = M.fromList updateDistances `M.union` distances
    unseen' = Q.fromList updateUnseen `Q.union` unseen
  in
    if Q.null unseen'
      then previous'
      else shortestPathTree' adjacencies previous' distances' unseen'
