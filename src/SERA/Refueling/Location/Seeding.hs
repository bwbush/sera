{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE RecordWildCards           #-}


module SERA.Refueling.Location.Seeding {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Parameters(..)
, Population
, Individual
, Bounds
, Point
, locate
, optimize
, iteratePopulation
) where


import Control.Monad (foldM, liftM, replicateM)
import Control.Monad.Random.Class (MonadRandom, getRandomR)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.List (sort, sortBy)
import "handa-data" Data.List.Util (regroupBy)
import GHC.Generics (Generic)
import SERA.Refueling.Location (Point2(..), Vector2(..), jitter', pvAdd, pManhattanDistance, rApplyR, rPoint, rPoints)
import SERA.Refueling.Location.Grid (Grid(..), gridValue)


data Parameters =
  Parameters
  {
    initialLocations     :: [Point]
  , locationCounts       :: [Int]
  , cutoffDistance       :: Int
  , locationWeights      :: [Double]
  , populationSize       :: Int
  , randomizeProbability :: Double
  , jitterProbability    :: Double
  , jitterDistance       :: Int
  , keepCount            :: Int
  , iterationLimit       :: Int
  }
    deriving (Generic, Read, Show)

instance FromJSON Parameters

instance ToJSON Parameters where
  toJSON = genericToJSON defaultOptions


type Point = Point2 Int


type Bounds = (Point, Point)


type Individual = [Point]


type Population = [(Individual, Double)]


locate :: forall g. MonadRandom g => Parameters -> Bounds -> Grid -> [g Individual]
locate parameters@Parameters{..} bounds grid =
  foldl batch [return initialLocations ] locationCounts
    where
      batch :: forall h. MonadRandom h => [h Individual] -> Int -> [h Individual]
      batch previousLocations locationCount =
        previousLocations ++
          [
            do
              previousLocations' <- sequence previousLocations
              let
                parameters' =
                  parameters
                  {
                    initialLocations = concat previousLocations'
                  , locationCounts   = [locationCount]
                  }
              optimize parameters' bounds grid
          ]


optimize :: MonadRandom g => Parameters -> Bounds -> Grid -> g Individual
optimize = ((fmap (fst . head) .) .) . iteratePopulation
    

iteratePopulation :: MonadRandom g => Parameters -> Bounds -> Grid -> g Population
iteratePopulation parameters@Parameters{..} bounds grid =
  do
    start <- initializePopulation parameters bounds grid
    foldM (const . propagatePopulation parameters bounds grid) start [1..iterationLimit]


initializePopulation :: MonadRandom g => Parameters -> Bounds -> Grid -> g Population
initializePopulation parameters@Parameters{..} bounds grid =
  map (evaluateIndividual parameters grid)
    <$> replicateM populationSize
    $ rPoints bounds
    $ head locationCounts


propagatePopulation :: MonadRandom g => Parameters -> Bounds -> Grid -> Population -> g Population
propagatePopulation parameters@Parameters{..} bounds grid population =
  do
    let
      newCount = populationSize - keepCount
      best = take keepCount $ sortBy ((. snd) . flip compare . snd) population
    new <- mapM (mutateIndividual parameters bounds)
      $ take newCount
      $ concat
      $ replicate ((newCount + keepCount - 1) `div` keepCount)
      $ map fst best
    return $ best ++ parMap rdeepseq (evaluateIndividual parameters grid) new


mutateIndividual :: MonadRandom g => Parameters -> Bounds -> Individual -> g Individual
mutateIndividual Parameters{..} bounds =
  rApplyR mutate probability
    where
      probability = randomizeProbability + jitterProbability
      mutate point =
        do
          p <- getRandomR (0, probability)
          if p < randomizeProbability
            then rPoint bounds
            else jitter' bounds jitterDistance point


evaluateIndividual :: Parameters -> Grid -> Individual -> (Individual, Double)
evaluateIndividual =
  if False
    then evaluateIndividual'
    else evaluateIndividual''


evaluateIndividual' :: Parameters -> Grid -> Individual -> (Individual, Double)
evaluateIndividual' Parameters{..} grid points =
  let
    coverage :: [(Point, [Double])]
    coverage = -- FIXME: this is the most time-consuming fragment of code, with about 65% of the runtime
      regroupBy (zipWith const locationWeights)
      [
        (point', (point' `pManhattanDistance` point, point))
      |
        point <- points ++ initialLocations
      , offset <- neighborhood cutoffDistance 
      , let point' = point `pvAdd` offset
      ]
  in
    (points, sum $ map (\(p, w) -> gridValue (unpoint2 p) grid * sum w) coverage)


evaluateIndividual'' :: Parameters -> Grid -> Individual -> (Individual, Double)
evaluateIndividual'' Parameters{..} grid points =
  let
    coverage :: [Point]
    coverage =
      sort
        [
          point `pvAdd` offset
        |
          point <- points ++ initialLocations
        , offset <- neighborhood cutoffDistance 
        ]
    nMax = length locationWeights
    tally :: Int -> Double -> [Point] -> Double -> Double
    tally _ _ [] z = z
    tally n w (p@(Point2 p') : ps) z =
      let
        !n' =
          if null ps || p /= head ps
            then 0
            else n + 1
        !w' =
          if n == 0
            then gridValue p' grid
            else w
        !z' =
          if n >= nMax
            then 0
            else z + w' * locationWeights !! n
      in
        tally n' w' ps z'
  in
    (points, tally 0 undefined coverage 0)


neighborhood :: (Enum a, Num a) => a -> [Vector2 a]
neighborhood range =
  [
    Vector2 (dx, dy)
  |
    dx <- [(-range)..range]
  , let range' = abs dx
  , dy <- [(-range')..range']
  ]
