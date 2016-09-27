{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Refueling.Hydrogen.Sizing (
  InfrastructureUtilizationParameters(..)
, maximumInfrastructureUtilization
, StationCapacityParameters
, stationCapacitiesByAverage
, stationCapacitiesByQuantile
, stationCapacities
, stationCounts
, stationCount
, stationCapacity
, relativeStationCapacity
, fractionStationCapacities
, relativeStationCapacityFit
) where


import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default(..))
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Math.Series (deltas, sums)
import Statistics.Distribution (cumulative, quantile)
import Statistics.Distribution.Gamma (GammaDistribution, gammaDistr)


data InfrastructureUtilizationParameters =
  InfrastructureUtilizationParameters
    {
      p0 :: Double
    , r  :: Double
    , k  :: Double
    }
      deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON InfrastructureUtilizationParameters

instance ToJSON InfrastructureUtilizationParameters

instance Default InfrastructureUtilizationParameters where
  def =
    InfrastructureUtilizationParameters
    {
      p0 = 0.307568
    , r  = 0.309753554701952
    , k  = 0.70 -- 0.81
    }


maximumInfrastructureUtilization :: InfrastructureUtilizationParameters -> Double -> Double
maximumInfrastructureUtilization InfrastructureUtilizationParameters{..} years =
  k * p0 * exp (r * years)
  / (k + p0 * (exp (r * years) - 1))


data StationCapacityParameters =
  StationCapacityParameters
    {
      alpha   :: Double
    , beta    :: Double
    , base    :: Double
    , qMax    :: Double
    , qAbsMin :: Double
    , qAbsMax :: Double
    }
      deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON StationCapacityParameters

instance ToJSON StationCapacityParameters

instance Default StationCapacityParameters where
  def =
    StationCapacityParameters
    {
      alpha   = 2.66106
    , beta    = 0.36723
    , base    = 2.7
    , qMax    = 2500 -- 1266.446529 -- 2500
    , qAbsMin = 100
    , qAbsMax = 8000
    }


sizeToBounds :: StationCapacityParameters -> Double -> Double
sizeToBounds StationCapacityParameters{..} q = maximum [qAbsMin, minimum [qAbsMax, q]]


stationCapacitiesByAverage :: StationCapacityParameters -> Double -> [Double] -> [(Int, Double)]
stationCapacitiesByAverage stationCapacityParameters initialStations demands =
  let
    utilizations =
      map (maximumInfrastructureUtilization def) [0..]
    totalCapacity =
      zipWith (/) demands utilizations
    newCapacity =
      deltas totalCapacity
    totalStations =
      map snd
      $ stationCounts stationCapacityParameters (head demands, initialStations)
      $ tail
      $ deltas demands
    newStations =
      deltas totalStations
    averageCapacity =
      map (sizeToBounds stationCapacityParameters)
        $ zipWith (/) newCapacity newStations
    newStations' =
      deltas
        $ map ceiling
        $ sums
        $ map (maximum . (: [0]))
        $ zipWith (/) newCapacity averageCapacity
    averageCapacity' =
      map (sizeToBounds stationCapacityParameters)
        $ zipWith (/) newCapacity
        $ map fromIntegral newStations'
    utilizations' =
      zipWith (/) demands
        $ sums
        $ zipWith (*) averageCapacity'
        $ map fromIntegral newStations'
    adjustments =
      zipWith (\utilization utilization' -> if utilization < utilization' then 1 else 0) utilizations utilizations'
    newStations'' =
      zipWith (+) newStations' adjustments
    averageCapacity'' =
      map (sizeToBounds stationCapacityParameters)
        $ zipWith (/) newCapacity
        $ map fromIntegral newStations''
  in
    zip newStations'' averageCapacity''


stationCapacitiesByQuantile :: StationCapacityParameters -> Double -> [Double] -> [[Double]]
stationCapacitiesByQuantile stationCapacityParameters =
  (map (map (sizeToBounds stationCapacityParameters)) .)
  . (map (uncurry (stationCapacities stationCapacityParameters) . swap) .)
  . stationCapacitiesByAverage stationCapacityParameters


stationCounts :: StationCapacityParameters -> (Double, Double) -> [Double] -> [(Double, Double)]
stationCounts stationCapacityParameters =
  (reverse .)
  . foldl (flip =<< ((:) .) . stationCount stationCapacityParameters . head)
  . return


stationCount :: StationCapacityParameters -> (Double, Double) -> Double -> (Double, Double)
stationCount StationCapacityParameters{..} (previousDemand, previousStations) newDemand =
  let
    demand = previousDemand + newDemand
    qAve = previousDemand / previousStations
    beta' = base**(qAve / qMax - 1)
    nStar = newDemand / qAve
    newStations = beta' * nStar
  in
    (demand, previousStations + newStations)


stationCapacities :: StationCapacityParameters -> Double -> Int -> [Double]
stationCapacities stationCapacityParameters averageCapacity count =
  map (stationCapacity stationCapacityParameters averageCapacity . (/ fromIntegral (count + 1)) . fromIntegral) [1..count]

 
stationCapacity :: StationCapacityParameters -> Double -> Double -> Double
stationCapacity stationCapacityParameters averageCapacity fraction = averageCapacity * relativeStationCapacity stationCapacityParameters fraction


relativeStationCapacity :: StationCapacityParameters -> Double -> Double
relativeStationCapacity stationCapacityParameters =
  quantile (relativeStationCapacityDistribution stationCapacityParameters)


fractionStationCapacities :: StationCapacityParameters -> Double -> (Double, Double) -> Double
fractionStationCapacities stationCapacityParameters averageCapacity (smallestCapacity, largestCapacity) =
  let
    pdf = relativeStationCapacityDistribution stationCapacityParameters
  in
    cumulative pdf (largestCapacity / averageCapacity)- cumulative pdf (smallestCapacity / averageCapacity)


relativeStationCapacityDistribution :: StationCapacityParameters -> GammaDistribution
relativeStationCapacityDistribution StationCapacityParameters{..} = gammaDistr alpha beta


relativeStationCapacityFit :: Double -> Double
relativeStationCapacityFit fraction =
  (
    264.1
    - 8.921 * percent
    + 0.1947 * percent^(2 :: Int)
    - 0.002187 * percent^(3 :: Int)
    + 0.000008948 * percent^(4 :: Int)
  ) / 100
    where
      percent = 100 * fraction
