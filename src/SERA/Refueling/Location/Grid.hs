{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Refueling.Location.Grid {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Grid(gridBounds, gridRange)
, GridIndex
, GridCell(..)
, gridLookup
, gridCenter
, gridValue
, readGrid
, gridSpacings
, gridSpacing
) where


import Control.Arrow ((***), (&&&))
import Data.List.Split (splitOn)
import SERA.Refueling.Location (Point2(..), Vector2(..))

import qualified Data.HashMap.Strict as M (HashMap, fromList, lookup)
import qualified Data.Vector as V (Vector, (!), generate)


type GridIndex = (Int, Int)


data GridCell =
  GridCell
  {
    cellCenter :: ! (Point2 Double)
  , cellWeight :: ! Double
  }
    deriving (Eq, Read, Show)


data Grid =
  Grid
  {
    gridBounds :: (GridIndex, GridIndex)
  , gridRange  :: (Point2 Double, Point2 Double)
  , gridCells  :: V.Vector (Maybe GridCell)
  }
    deriving (Read, Show)


gridCenter :: GridIndex -> Grid -> Maybe (Point2 Double)
gridCenter point grid =
  cellCenter <$> point `gridLookup` grid


gridValue :: GridIndex -> Grid -> Double
gridValue point grid =
  maybe 0 cellWeight $ point `gridLookup` grid


gridLookup :: GridIndex -> Grid -> Maybe GridCell
gridLookup point Grid{..} = 
  if point `insideGrid` gridBounds
    then gridCells V.! (point `fromGridIndex` gridBounds)
    else Nothing


insideGrid :: GridIndex -> (GridIndex, GridIndex) -> Bool
insideGrid (row, col) ((rowMin, colMin), (rowMax, colMax)) =
  row >= rowMin && row <= rowMax && col >= colMin && col <= colMax


fromGridIndex :: GridIndex -> (GridIndex, GridIndex) -> Int
fromGridIndex (row, col) ((rowMin, colMin), (_, colMax)) =
  (row - rowMin) * stride + col - colMin
    where
      stride = colMax - colMin + 1


toGridIndex :: Int -> (GridIndex, GridIndex) -> GridIndex
toGridIndex index ((rowMin, colMin), (_, colMax)) =
  (rowMin +) *** (colMin +) $ index `divMod` stride
    where
      stride = colMax - colMin + 1


readGrid :: FilePath -> IO Grid
readGrid path =
  do
    let
      parse :: [String] -> (GridIndex, GridCell)
      parse [row, column, longitude, latitude, weight] =
        (
          (read row, read column)
        , GridCell
          (Point2 (read longitude, read latitude))
          (read weight)
        )
      parse s = error $ "SERA.Location.Grid: parse failed for \"" ++ show s ++ "\"."
    grid <- (map (parse . splitOn ",") . tail . lines) <$> readFile path
    let
      !gridBounds@(_, lastIndex) =
        (minimum *** minimum) &&& (maximum *** maximum)
          $ unzip
          $ map fst grid
      !gridRange =
        (Point2 . (minimum *** minimum)) &&& (Point2 . (maximum *** maximum))
          $ unzip
          $ map (unpoint2 . cellCenter . snd) grid
      grid' :: M.HashMap GridIndex GridCell
      grid' = M.fromList grid
      !gridCells =
        V.generate (lastIndex `fromGridIndex` gridBounds + 1) $
        (`M.lookup` grid') . (`toGridIndex` gridBounds)
    return Grid{..}


gridSpacings :: Grid -> Vector2 Double
gridSpacings Grid{..} =
  let
     ((ix0, iy0), (ix1, iy1)) = gridBounds
     (Point2 (cx0, cy0), Point2 (cx1, cy1)) = gridRange
  in
    Vector2 ((cx1 - cx0) / fromIntegral (ix1 - ix0), (cy1 - cy0) /  fromIntegral (iy1 - iy0))


gridSpacing :: Grid -> Double
gridSpacing grid =
  let
    (Vector2 (dx, dy)) = gridSpacings grid
  in
    sqrt $ dx^(2 :: Int) + dy^(2 :: Int)
