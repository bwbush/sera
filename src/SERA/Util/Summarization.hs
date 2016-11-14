module SERA.Util.Summarization {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Summable(..)
, Totalable(..)
, sumEach
) where


class Totalable a where
  total :: a -> Double


class Summable a where
  summation :: [a] -> a


sumEach :: Num a => [[a]] -> [a]
sumEach [] = []
sumEach x =
  let
    x' = filter (not . null) x
  in
    if null x'
      then []
      else sum (map head x') : sumEach (map tail x')
