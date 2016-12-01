{-# LANGUAGE DeriveGeneric #-}


module SERA.Refueling.Location {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Point2(..)
, Vector2(..)
, pBounds
, pvAdd
, pSubtract
, rPoint
, rPoints
, jitter
, jitter'
, constrain
, vScale
, pManhattanDistance
, vManhattanDistance
, rVector
, rApply
, rApplyR
) where


import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Control.Monad.Random (Random, getRandomR, getRandomRs, getRandom, getRandoms)
import Control.Monad.Random.Class (MonadRandom)
import Control.Parallel.Strategies (NFData)
import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import GHC.Generics (Generic)


newtype Point2 a = Point2 {unpoint2 :: (a, a)}
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData a => NFData (Point2 a)

instance FromJSON a => FromJSON (Point2 a)

instance ToJSON a => ToJSON (Point2 a) where
  toJSON = genericToJSON defaultOptions


newtype Vector2 a = Vector2 {unvector :: (a, a)}
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData a => NFData (Vector2 a)

instance FromJSON a => FromJSON (Vector2 a)

instance ToJSON a => ToJSON (Vector2 a) where
  toJSON = genericToJSON defaultOptions


pBounds :: Ord a => [Point2 a] -> (Point2 a, Point2 a)
pBounds = minimum &&& maximum


pvAdd :: Num a => Point2 a -> Vector2 a -> Point2 a
pvAdd (Point2 (xp, yp)) (Vector2 (xv, yv)) = Point2 (xp + xv, yp + yv)


pSubtract :: Num a => Point2 a -> Point2 a -> Vector2 a
pSubtract (Point2 (x1, y1)) (Point2 (x2, y2)) = Vector2 (x1 - x2, y1 - y2)


rPoint :: (Random a, MonadRandom g) => (Point2 a, Point2 a) -> g (Point2 a)
rPoint (Point2 (x0, y0), Point2 (x1, y1)) =
  do
    x <- getRandomR (x0, x1)
    y <- getRandomR (y0, y1)
    return $ Point2 (x, y)


rPoints :: (Random a, MonadRandom g) => (Point2 a, Point2 a) -> Int -> g [Point2 a]
rPoints b n = replicateM n  $ rPoint b


jitter :: (Num a, Ord a, Random a, MonadRandom g) => a -> Point2 a -> g (Point2 a)
jitter s p =
  do
    rv <- rVector s
    return $ p `pvAdd` rv


jitter' :: (Num a, Ord a, Random a, MonadRandom g) => (Point2 a, Point2 a) -> a -> Point2 a -> g (Point2 a)
jitter' bounds s p = constrain bounds <$> jitter s p


constrain :: Ord a => (Point2 a, Point2 a) -> Point2 a -> Point2 a
constrain (Point2 (x0, y0), Point2 (x1, y1)) (Point2 (x, y)) =
  Point2 (maximum [x0, minimum [x, x1]], maximum [y0, minimum [y, y1]])


vScale :: Num a => a -> Vector2 a -> Vector2 a
vScale s (Vector2 (xv, yv)) = Vector2 (s * xv, s * yv)


pManhattanDistance :: Num a => Point2 a -> Point2 a -> a
pManhattanDistance = (vManhattanDistance .) . pSubtract


vManhattanDistance :: Num a => Vector2 a -> a
vManhattanDistance (Vector2 (x, y)) = abs x + abs y


rVector :: (Num a, Ord a, Random a, MonadRandom g) => a -> g (Vector2 a)
rVector s =
  do
    xs <- getRandomRs (-s, s)
    ys <- getRandomRs (-s, s)
    let
      (x', y') = head $ dropWhile (\(x, y) -> x * x + y * y > s * s) $ zip xs ys
    return $ Vector2 (x', y')


rApply :: MonadRandom g => (a -> a) -> Double -> [a] -> g [a]
rApply f alpha xs =
  do
    rs <- getRandoms
    return $ zipWith (\r x -> if r <= alpha then f x else x) rs xs


rApplyR :: MonadRandom g => (a -> g a) -> Double -> [a] -> g [a]
rApplyR f alpha xs =
  sequence
    [
      do
        r <- getRandom
        if r <= alpha
          then f x
          else return x
    |
      x <- xs
    ]
