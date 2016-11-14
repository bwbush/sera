{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE RecordWildCards           #-}


module SERA.Refueling.Location.Progressive {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Parameters(..)
, Bounds
, Point
, locate
) where


import Control.Arrow (second)
import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.List.Split (splitPlaces)
import "handa-data" Data.List.Util (regroup)
import GHC.Generics (Generic)
import SERA.Refueling.Location (Point2(..), Vector2(..), pvAdd, pManhattanDistance)
import SERA.Refueling.Location.Grid (Grid(..), gridValue)


data Parameters =
  Parameters
  {
    initialLocations :: [Point]
  , locationCounts   :: [Int]
  , cutoffDistance   :: Int
  , reductionWeight  :: Double
  }
    deriving (Generic, Read, Show)

instance FromJSON Parameters

instance ToJSON Parameters where
  toJSON = genericToJSON defaultOptions


type Point = Point2 Int


type Bounds = (Point, Point)


locate :: Parameters -> Bounds -> Grid -> [[Point]]
locate parameters@Parameters{..} bounds grid =
  splitPlaces locationCounts
    $ evalState (replicateM (sum locationCounts) $ nextPoint parameters)
    $ foldl (updateValues parameters) (computeValues parameters bounds grid) initialLocations


nextPoint :: Parameters -> State Values Point
nextPoint parameters =
  state $ \values ->
    let
      point = maximumPoint values
      values' = updateValues parameters values point
    in
      (point, values')


  
type Value = (Point, Double)


type Values = [Value]


computeValues :: Parameters -> Bounds -> Grid -> Values
computeValues Parameters{..} (bounds0, bounds1) grid =
  filter ((> 0) . flip gridValue grid . unpoint2 . fst)
    $ map (second sum) $ regroup
      [
        (point', value) 
      |
        let ((col0, row0), (col1, row1)) = gridBounds grid
      , col <- [col0..col1]
      , row <- [row0..row1]
      , let point = Point2 (col, row)
      , let value = unpoint2 point `gridValue` grid
      , value > 0
      , offset <- neighborhood cutoffDistance
      , let point' = point `pvAdd` offset
      , point' >= bounds0
      , point' <= bounds1
      ]


maximumPoint :: Values -> Point
maximumPoint =
  fst . foldl1 maximumPoint'
    where
      maximumPoint' :: Value -> Value -> Value
      maximumPoint' pv@(_, v) pv'@(_, v') =
        if v < v'
          then pv'
          else pv


updateValues :: Parameters -> Values -> Point -> Values
updateValues Parameters{..} values point =
  flip map values $ \pointValue@(point', _) ->
    second
      (
        let
          d = point `pManhattanDistance` point'
        in
          if d <= cutoffDistance
            then (* (1 - reductionWeight^d))
            else id
      )
      pointValue


neighborhood :: (Enum a, Num a) => a -> [Vector2 a]
neighborhood range =
  [
    Vector2 (dx, dy)
  |
    dx <- [(-range)..range]
  , let range' = abs dx
  , dy <- [(-range')..range']
  ]
