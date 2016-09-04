-----------------------------------------------------------------------------
--
-- Module      :  SERA.Vehicle.MHD.Census2002
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Medium and heavy duty vehicle data from the Census Bureau, circa 2002.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module SERA.Vehicle.MHD.Census2002 (
-- * Vehicle types
  vehicles
-- * Tabulated functions
, annualTravel
, fuelEfficiency
-- * Raw data
, table
) where


import Data.Daft.Vinyl.FieldCube (type (↝), fromRecords)
import Data.Daft.Vinyl.FieldRec ((<:), readFieldRecs)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (rcast)
import SERA.Vehicle.Types (FAnnualTravel, FFuelEfficiency, Vehicle, FVehicle, fVehicle)


-- | Vehicle types.
vehicles :: [Vehicle]
vehicles = (fVehicle <:) <$> table


-- | Annual travel.
annualTravel :: '[FVehicle] ↝ '[FAnnualTravel]
annualTravel = fromRecords $ rcast <$> table


-- | Fuel efficiency.
fuelEfficiency :: '[FVehicle] ↝ '[FFuelEfficiency]
fuelEfficiency = fromRecords $ rcast <$> table


-- | Raw data.
-- |
-- | Source: U.S. Department of Commerce, Bureau of the Census, 2002 Vehicle Inventory and Use Survey, Microdata File on CD, 2005.  (Additional resources:  www.census.gov/svsd/www.tiusview.html)
table :: [FieldRec '[FVehicle, FAnnualTravel, FFuelEfficiency]]
Right table =
  readFieldRecs
    [
      ["Vehicle", "Annual Travel [mi/yr]", "Fuel Efficiency [mi/gge]"]
    , ["Class 1",                 "11822",                     "17.6"]
    , ["Class 2",                 "12684",                     "14.3"]
    , ["Class 3",                 "14094",                     "10.5"]
    , ["Class 4",                 "15441",                      "8.5"]
    , ["Class 5",                 "11645",                      "7.9"]
    , ["Class 6",                 "12671",                      "7.0"]
    , ["Class 7",                 "30708",                      "6.4"]
    , ["Class 8",                 "45739",                      "5.7"]
    ]
  :: Either String [FieldRec '[FVehicle, FAnnualTravel, FFuelEfficiency]]
