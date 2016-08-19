module SERA.Vehicle.MHD.TEDB32 (
  classifications
, fuelEconomyRegressed
, fuelEconomyTabulated
) where


import Control.Arrow (first, second)
import Data.Daft.Lookup (LookupTable, asLookupTable, keys, lookupOrd, lookupReal)
import Data.List.Util (sortedGroups)
import SERA.Vehicle.Stock.Types (FuelEconomyFunction)
import SERA.Vehicle.Types (Classification(Classification), FuelEconomy, ModelYear)


classifications :: [Classification]
classifications = keys regressions5p1and5p2


fuelEconomyRegressed :: FuelEconomyFunction
fuelEconomyRegressed _region classification modelYear _fuel =
  let
    (m, b) = classification `lookupOrd` regressions5p1and5p2
  in
    m * fromIntegral modelYear + b


regressions5p1and5p2 :: LookupTable Classification (Double, Double)
regressions5p1and5p2 =
  asLookupTable
    $ fmap (first (Classification . ("Class " ++) . show))
    [
      (3 :: Int, (0.0771, -142.87))
    , (4       , (0.0624, -115.65))
    , (5       , (0.058 , -107.49))
    , (6       , (0.0514, -95.245))
    , (7       , (0.0244, -41.26 ))
    , (8       , (0.0196, -33.573))
    ]


fuelEconomyTabulated :: FuelEconomyFunction
fuelEconomyTabulated _region classification modelYear _fuel = modelYear `lookupReal` (classification `lookupOrd` tables5p1and5p2)


tables5p1and5p2 :: LookupTable Classification (LookupTable ModelYear FuelEconomy)
tables5p1and5p2 =
  asLookupTable
    . fmap (second asLookupTable)
    $ sortedGroups
    [
      (classification, (modelYear - 1, fuelEconomy))
    |
      (modelYear, fuelEconomies) <- rawData
    , (classification, fuelEconomy) <- zip classifications fuelEconomies
    ]
    where
      rawData =
        [
          (1970, [ 9.02,  7.28,  6.77,  6.01,  6.81,  5.04])
        , (1971, [ 9.09,  7.34,  6.83,  6.06,  6.83,  5.06])
        , (1972, [ 9.17,  7.40,  6.89,  6.12,  6.86,  5.08])
        , (1973, [ 9.25,  7.47,  6.94,  6.17,  6.88,  5.10])
        , (1974, [ 9.33,  7.53,  7.00,  6.22,  6.91,  5.12])
        , (1975, [ 9.40,  7.59,  7.06,  6.27,  6.93,  5.14])
        , (1976, [ 9.48,  7.65,  7.12,  6.32,  6.95,  5.16])
        , (1977, [ 9.56,  7.71,  7.18,  6.37,  6.98,  5.18])
        , (1978, [ 9.63,  7.78,  7.23,  6.42,  7.00,  5.20])
        , (1979, [ 9.71,  7.84,  7.29,  6.48,  7.03,  5.22])
        , (1980, [ 9.79,  7.90,  7.35,  6.53,  7.05,  5.24])
        , (1981, [ 9.87,  7.96,  7.41,  6.58,  7.08,  5.25])
        , (1982, [ 9.94,  8.03,  7.47,  6.63,  7.10,  5.27])
        , (1983, [10.02,  8.09,  7.52,  6.68,  7.13,  5.29])
        , (1984, [10.10,  8.15,  7.58,  6.73,  7.15,  5.31])
        , (1985, [10.17,  8.21,  7.64,  6.78,  7.17,  5.33])
        , (1986, [10.25,  8.28,  7.70,  6.84,  7.20,  5.35])
        , (1987, [10.33,  8.34,  7.76,  6.89,  7.22,  5.37])
        , (1988, [10.40,  8.40,  7.81,  6.94,  7.25,  5.39])
        , (1989, [10.48,  8.46,  7.87,  6.99,  7.27,  5.41])
        , (1990, [10.56,  8.53,  7.93,  7.04,  7.30,  5.43])
        , (1991, [10.64,  8.59,  7.99,  7.09,  7.32,  5.45])
        , (1992, [10.71,  8.65,  8.05,  7.14,  7.34,  5.47])
        , (1993, [10.79,  8.71,  8.10,  7.20,  7.37,  5.49])
        , (1994, [10.87,  8.78,  8.16,  7.25,  7.39,  5.51])
        , (1995, [10.94,  8.84,  8.22,  7.30,  7.42,  5.53])
        , (1996, [11.02,  8.90,  8.28,  7.35,  7.44,  5.55])
        , (1997, [11.10,  8.96,  8.34,  7.40,  7.47,  5.57])
        , (1998, [11.18,  9.03,  8.39,  7.45,  7.49,  5.59])
        , (1999, [11.25,  9.09,  8.45,  7.50,  7.52,  5.61])
        , (2000, [11.33,  9.15,  8.51,  7.55,  7.54,  5.63])
        , (2001, [11.41,  9.21,  8.57,  7.61,  7.56,  5.65])
        , (2002, [11.48,  9.27,  8.63,  7.66,  7.59,  5.67])
        , (2003, [11.56,  9.34,  8.68,  7.71,  7.61,  5.69])
        , (2004, [11.64,  9.40,  8.74,  7.76,  7.64,  5.71])
        , (2005, [11.72,  9.46,  8.80,  7.81,  7.66,  5.73])
        , (2006, [11.79,  9.52,  8.86,  7.86,  7.69,  5.74])
        , (2007, [11.87,  9.59,  8.92,  7.91,  7.71,  5.76])
        , (2008, [11.95,  9.65,  8.97,  7.97,  7.74,  5.78])
        , (2009, [12.02,  9.71,  9.03,  8.02,  7.76,  5.80])
        , (2010, [12.10,  9.77,  9.09,  8.07,  7.78,  5.82])
        , (2011, [12.18,  9.84,  9.15,  8.12,  7.81,  5.84])
        , (2012, [12.26,  9.90,  9.21,  8.17,  7.83,  5.86])
        , (2013, [12.33,  9.96,  9.26,  8.22,  7.86,  5.88])
        , (2014, [12.41, 10.02,  9.32,  8.27,  7.88,  5.90])
        ]
