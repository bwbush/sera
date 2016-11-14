{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module SERA.Finance.Capital.Stations {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  stationSizing
, sizeStations
, makeStations
) where


import Control.Arrow (first, second)
import Data.Default (def)
import Data.Default.Util (nan)
import Data.List (nub)
import Data.List.Util (regroup)
import Data.Maybe (fromMaybe)
import Data.Table.Identifier (Id(..))
import SERA.Finance.Capital (Quantity, Capital(..), Operations, ageStationTo, combineStations, costStation)
import SERA.Finance.Demand (ProjectDemand(..), ProjectDemands, unProjectDemands)
import SERA.Finance.Project (Project)
import SERA.Finance.Types (StationSizing(..))
import SERA.Refueling.FCVSE.Sizing (stationCapacitiesByQuantile)
import SERA.Util.Time (Year(..))

import qualified SERA.Finance.Demand as D (Demand(..))


stationSizing :: ProjectDemands -> [StationSizing]
stationSizing =
  let
    f :: ((Project, Year), [Double]) -> [StationSizing]
    f (k, xx) = zipWith (g k) [1..] xx
    g :: (Project, Year) -> Int -> Double -> StationSizing
    g (r, y) i x = StationSizing ((r, y, Id i), x)
  in
    concatMap f . {- map laCorrection . -} sizeStations


-- FIXME: move this to a file
laCorrection :: ((Project, Year), [Double]) -> ((Project, Year), [Double])
laCorrection ((region, year), capacities) =
  let
    corrections :: [(Year, Int)]
    corrections = zipWith ((,) . Year) [2013..] [5, 9, 17, 6, 7, 6, 7, 3]
    count' :: Int
    count' = fromMaybe (length capacities) $ lookup year corrections
    capacities' = take count' $ capacities ++ [100..]
  in
    ((region, year), capacities')


sizeStations :: ProjectDemands -> [((Project, Year), [Double])]
sizeStations demands =
  concatMap (`sizeStations'` demands)
    $ nub
    $ map (fst . fst . unProjectDemand) demands


sizeStations' :: Project -> ProjectDemands -> [((Project, Year), [Double])]
sizeStations' region demands =
  let
    demands' :: [(Year, Double)]
    demands' = map (first snd . second D.hydrogenDemand) $ filter ((== region) . fst . fst) $ unProjectDemands demands
    years :: [Year]
    years = map fst demands'
    years' = enumFromTo (minimum years) (maximum years)
    demands'' :: [Double]
    demands'' = [fromMaybe 0 $ lookup year demands' | year <- years']
    initial :: Double
    initial = fromIntegral $ (ceiling :: Double -> Int) $ (/ 12.5) $ head $ filter (/= 0) demands'' ++ [nan]
    sizes :: [[Double]]
    sizes =
      map (map (\x -> maximum [x, 100]))
        $ stationCapacitiesByQuantile def initial demands''
  in
    zipWith ((,) . (region, )) years' sizes


makeStations :: Capital -> Operations -> [(Year, Double)] -> [StationSizing] -> [(Project, [(Year, Capital)])]
makeStations characteristics operations cumulativeDemands sizings =
  let
    schedules :: [(Project, (Year, Double))]
    schedules = map (\(StationSizing ((r, y, _), q)) -> (r, (y, q))) sizings
    schedules' :: [(Project, [(Year, Double)])]
    schedules' = regroup schedules
    schedules'' :: [(Project, [(Year, [Double])])]
    schedules'' = mapSecond regroup schedules'
    finalYear :: Int
    finalYear = maximum $ map (unYear . fst . snd) schedules
--  capacityAdditions :: [(Year, Double)]
--  capacityAdditions = undefined
  in
    mapSecond (makeStations' characteristics operations cumulativeDemands finalYear) schedules''


makeStations' :: Capital -> Operations -> [(Year, Double)] -> Int -> [(Year, [Double])] -> [(Year, Capital)]
makeStations' characteristics operations cumulativeDemands finalYear schedule =
  let
    schedule' :: [(Year, Quantity, [Double])]
    schedule' =
      [
        (year, fromMaybe nan $ year `lookup` cumulativeDemands, demands)
      |
        (year, demands) <- schedule
      ]
    stations :: [Capital]
    stations = concatMap (makeStations'' characteristics operations finalYear) schedule'
    stations' :: [(Year, [Capital])]
    stations' = regroup $ map (\station@Station{..} -> (Year stationYear, station)) stations
  in
    mapSecond combineStations stations'


mapSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSecond = map . second


makeStations'' :: Capital -> Operations -> Int -> (Year, Quantity, [Double]) -> [Capital]
makeStations'' characteristics operations finalYear (Year initialYear, quantity, capacities) =
  concatMap (makeStation characteristics operations quantity initialYear finalYear) capacities


makeStation :: Capital -> Operations -> Quantity -> Int -> Int -> Double -> [Capital]
makeStation characteristics operations quantity initialYear finalYear capacity =
  ageStationTo
    operations
    (
      costStation
        quantity
        operations
        initialYear
        characteristics {stationCapacity = capacity}
    )
    finalYear
