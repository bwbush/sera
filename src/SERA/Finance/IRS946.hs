module SERA.Finance.IRS946 {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Quarter
, quarters
, RecoveryPeriod
, recoveryPeriods
, depreciate
) where


import Data.Default.Util (nan)
import Data.List (transpose)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M (fromList, lookup)

type Quarter = Int


quarters :: [Quarter]
quarters = [1..4]


type RecoveryPeriod = Int


recoveryPeriods :: [RecoveryPeriod]
recoveryPeriods = [3, 5, 7, 10, 15, 20]


depreciate :: Quarter -> RecoveryPeriod -> [Double]
depreciate quarter recoveryPeriod =
  fromMaybe [nan]
    $ (recoveryPeriod `M.lookup`)
    =<< quarter `M.lookup` table


table :: Map Quarter (Map RecoveryPeriod [Double])
table =
  M.fromList [
    (
      quarters !! quarter
    , M.fromList [
        (
          recoveryPeriods !! recoveryPeriod
        , map (/ 100)
            $ takeWhile (> 0)
            $ transpose (rawTable !! quarter) !! recoveryPeriod
        )
      |
        recoveryPeriod <- [0..(length recoveryPeriods - 1)]
      ]
    )
  |
    quarter <- [0..(length quarters - 1)]
  ]



rawTable :: [[[Double]]]
rawTable =
  [
    [
      [58.330, 35.000, 25.000, 17.500,  8.750,  6.563]
    , [27.780, 26.000, 21.430, 16.500,  9.130,  7.000]
    , [12.350, 15.600, 15.310, 13.200,  8.210,  6.482]
    , [ 1.540, 11.010, 10.930, 10.560,  7.390,  5.996]
    , [     0, 11.010,  8.750,  8.450,  6.650,  5.546]
    , [     0,  1.380,  8.740,  6.760,  5.990,  5.130]
    , [     0,      0,  8.750,  6.550,  5.900,  4.746]
    , [     0,      0,  1.090,  6.550,  5.910,  4.459]
    , [     0,      0,      0,  6.560,  5.900,  4.459]
    , [     0,      0,      0,  6.550,  5.910,  4.459]
    , [     0,      0,      0,  0.820,  5.900,  4.459]
    , [     0,      0,      0,      0,  5.910,  4.460]
    , [     0,      0,      0,      0,  5.900,  4.459]
    , [     0,      0,      0,      0,  5.910,  4.460]
    , [     0,      0,      0,      0,  5.900,  4.459]
    , [     0,      0,      0,      0,  0.740,  4.460]
    , [     0,      0,      0,      0,      0,  4.459]
    , [     0,      0,      0,      0,      0,  4.460]
    , [     0,      0,      0,      0,      0,  4.459]
    , [     0,      0,      0,      0,      0,  4.460]
    , [     0,      0,      0,      0,      0,  0.565]
    ] , [
      [41.670, 25.000, 17.850, 12.500,  6.250,  4.688]
    , [38.890, 30.000, 23.470, 17.500,  9.380,  7.148]
    , [14.140, 18.000, 16.760, 14.000,  8.440,  6.612]
    , [ 5.300, 11.370, 11.970, 11.200,  7.590,  6.116]
    , [     0, 11.370,  8.870,  8.960,  6.830,  5.658]
    , [     0,  4.260,  8.870,  7.170,  6.150,  5.233]
    , [     0,      0,  8.870,  6.550,  5.910,  4.841]
    , [     0,      0,  3.340,  6.550,  5.900,  4.478]
    , [     0,      0,      0,  6.560,  5.910,  4.463]
    , [     0,      0,      0,  6.550,  5.900,  4.463]
    , [     0,      0,      0,  2.460,  5.910,  4.463]
    , [     0,      0,      0,      0,  5.900,  4.463]
    , [     0,      0,      0,      0,  5.910,  4.463]
    , [     0,      0,      0,      0,  5.900,  4.463]
    , [     0,      0,      0,      0,  5.910,  4.462]
    , [     0,      0,      0,      0,  2.210,  4.463]
    , [     0,      0,      0,      0,      0,  4.462]
    , [     0,      0,      0,      0,      0,  4.463]
    , [     0,      0,      0,      0,      0,  4.462]
    , [     0,      0,      0,      0,      0,  4.463]
    , [     0,      0,      0,      0,      0,  1.673]
    ] , [
      [25.000, 15.000, 10.710,  7.500,  3.750,  2.813]
    , [50.000, 34.000, 25.510, 18.500,  9.630,  7.289]
    , [16.670, 20.400, 18.220, 14.800,  8.660,  6.742]
    , [ 8.330, 12.240, 13.020, 11.840,  7.800,  6.237]
    , [     0, 11.300,  9.300,  9.470,  7.020,  5.769]
    , [     0,  7.060,  8.850,  7.580,  6.310,  5.336]
    , [     0,      0,  8.860,  6.550,  5.900,  4.936]
    , [     0,      0,  5.530,  6.550,  5.900,  4.566]
    , [     0,      0,      0,  6.560,  5.910,  4.460]
    , [     0,      0,      0,  6.550,  5.900,  4.460]
    , [     0,      0,      0,  4.100,  5.910,  4.460]
    , [     0,      0,      0,      0,  5.900,  4.460]
    , [     0,      0,      0,      0,  5.910,  4.461]
    , [     0,      0,      0,      0,  5.900,  4.460]
    , [     0,      0,      0,      0,  5.910,  4.461]
    , [     0,      0,      0,      0,  3.690,  4.460]
    , [     0,      0,      0,      0,      0,  4.461]
    , [     0,      0,      0,      0,      0,  4.460]
    , [     0,      0,      0,      0,      0,  4.461]
    , [     0,      0,      0,      0,      0,  4.460]
    , [     0,      0,      0,      0,      0,  2.788]
    ] , [
      [ 8.330,  5.000,  3.570,  2.500,  1.250,  0.938]
    , [61.110, 38.000, 27.550, 19.500,  9.880,  7.430]
    , [20.370, 22.800, 19.680, 15.600,  8.890,  6.872]
    , [10.190, 13.680, 14.060, 12.480,  8.000,  6.357]
    , [     0, 10.940, 10.040,  9.980,  7.200,  5.880]
    , [     0,  9.580,  8.730,  7.990,  6.480,  5.439]
    , [     0,      0,  8.730,  6.550,  5.900,  5.031]
    , [     0,      0,  7.640,  6.550,  5.900,  4.654]
    , [     0,      0,      0,  6.560,  5.900,  4.458]
    , [     0,      0,      0,  6.550,  5.910,  4.458]
    , [     0,      0,      0,  5.740,  5.900,  4.458]
    , [     0,      0,      0,      0,  5.910,  4.458]
    , [     0,      0,      0,      0,  5.900,  4.458]
    , [     0,      0,      0,      0,  5.910,  4.458]
    , [     0,      0,      0,      0,  5.900,  4.458]
    , [     0,      0,      0,      0,  5.170,  4.458]
    , [     0,      0,      0,      0,      0,  4.458]
    , [     0,      0,      0,      0,      0,  4.459]
    , [     0,      0,      0,      0,      0,  4.458]
    , [     0,      0,      0,      0,      0,  4.459]
    , [     0,      0,      0,      0,      0,  3.901]
    ]
  ]