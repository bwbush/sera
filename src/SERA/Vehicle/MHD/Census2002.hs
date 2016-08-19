module SERA.Vehicle.MHD.Census2002 (
  classifications
, annualTravelFunction
, fuelEconomyFunction
) where


import Control.Arrow (first)
import Data.Daft.Lookup (LookupTable, asLookupTable, keys, lookupOrd)
import SERA.Vehicle.Stock.Types (AnnualTravelFunction, FuelEconomyFunction)
import SERA.Vehicle.Types (AnnualTravel, Classification(Classification), FuelEconomy)


classifications :: [Classification]
classifications = keys table


-- [mi/veh]
annualTravelFunction :: AnnualTravelFunction
annualTravelFunction _region classification _age _fuel = fst $ classification `lookupOrd` table


-- [mi/gge]
fuelEconomyFunction :: FuelEconomyFunction
fuelEconomyFunction _region classification _age _fuel = snd $ classification `lookupOrd` table


-- Source: U.S. Department of Commerce, Bureau of the Census, 2002 Vehicle Inventory and Use Survey, Microdata File on CD, 2005.  (Additional resources:  www.census.gov/svsd/www.tiusview.html)
table :: LookupTable Classification (AnnualTravel, FuelEconomy)
table =
  asLookupTable
    $ fmap (first $ Classification . ("Class " ++) . show)
    [
      (1 :: Int, (11822, 17.6))
    , (2       , (12684, 14.3))
    , (3       , (14094, 10.5))
    , (4       , (15441,  8.5))
    , (5       , (11645,  7.9))
    , (6       , (12671,  7.0))
    , (7       , (30708,  6.4))
    , (8       , (45739,  5.7))
    ]
