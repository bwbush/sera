-----------------------------------------------------------------------------
--
-- Module      :  SERA.Vehicle.MHD.EMFAC2010
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Medium and heavy duty vehicle data from the EMFAC, circa 2002.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module SERA.Vehicle.MHD.EMFAC2010 (
-- * Vehicle types
  vehicles
-- * Tabulated functions
, annualTravel
-- * Raw data
, table6p7
) where


import Data.Daft.Vinyl.FieldCube (type (↝), θ, fromRecords)
import Data.Daft.Vinyl.FieldRec.IO (readFieldRecs)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (rcast)
import SERA.Vehicle.Types (FAge, FAnnualTravel, Vehicle(..), FVehicle)


-- | Vehicle types.
vehicles :: [Vehicle]
vehicles = Vehicle . ("Class " ++) . show <$> ([8,7..3] :: [Int])


-- | Annual travel.
annualTravel :: '[FVehicle] ↝ '[FAnnualTravel]
annualTravel = θ . fromRecords $ rcast <$> table6p7


-- | Raw data from EMFAC 2010 Table 6.7.
table6p7 :: [FieldRec '[FVehicle, FAge, FAnnualTravel]]
Right table6p7 =
  readFieldRecs
    $ ["Vehicle", "Age [yr]", "Annual Travel [mi/yr]"]
    : [
         [show classification, show age, show distance]
      |
        (age, annualTravels) <- raw
      , (classification, distance) <- zip vehicles annualTravels
      ]
    :: Either String [FieldRec '[FVehicle, FAge, FAnnualTravel]]
    where
      raw :: [(Int, [Double])]
      raw =
        [
          ( 1, [80705, 54183, 22357, 20547, 27245, 24868])
        , ( 2, [85152, 57169, 23589, 21679, 28746, 26238])
        , ( 3, [86460, 58047, 23951, 22012, 29188, 26641])
        , ( 4, [85386, 57326, 23654, 21739, 28825, 26310])
        , ( 5, [82571, 55436, 22874, 21022, 27875, 25443])
        , ( 6, [78547, 52734, 21759, 19998, 26516, 24203])
        , ( 7, [73755, 49517, 20432, 18778, 24899, 22726])
        , ( 8, [68546, 46020, 18989, 17451, 23140, 21121])
        , ( 9, [63199, 42430, 17507, 16090, 21335, 19474])
        , (10, [57926, 38890, 16047, 14748, 19555, 17849])
        , (11, [52881, 35503, 14649, 13463, 17852, 16294])
        , (12, [48169, 32339, 13344, 12264, 16261, 14843])
        , (13, [43854, 29442, 12148, 11165, 14805, 13513])
        , (14, [39965, 26831, 11071, 10175, 13492, 12315])
        , (15, [36504, 24508, 10112,  9294, 12323, 11248])
        , (16, [33452, 22459,  9267,  8517, 11293, 10308])
        , (17, [30772, 20659,  8524,  7834, 10388,  9482])
        , (18, [28417, 19078,  7872,  7235,  9593,  8756])
        , (19, [26335, 17681,  7295,  6705,  8890,  8115])
        , (20, [24469, 16428,  6778,  6230,  8260,  7540])
        , (21, [22764, 15283,  6306,  5796,  7685,  7014])
        , (22, [21171, 14214,  5865,  5390,  7147,  6524])
        , (23, [19645, 13189,  5442,  5001,  6632,  6053])
        , (24, [18150, 12185,  5028,  4621,  6127,  5593])
        , (25, [16662, 11186,  4616,  4242,  5625,  5134])
        , (26, [15164, 10181,  4201,  3861,  5119,  4673])
        , (27, [13653,  9166,  3782,  3476,  4609,  4207])
        , (28, [12136,  8148,  3362,  3090,  4097,  3740])
        , (29, [10629,  7136,  2944,  2706,  3588,  3275])
        , (30, [9159 ,  6149,  2537,  2332,  3092,  2822])
        , (31, [7759 ,  5209,  2149,  1975,  2619,  2391])
        , (32, [6467 ,  4342,  1791,  1646,  2183,  1993])
        , (33, [5324 ,  3574,  1475,  1355,  1797,  1641])
        , (34, [4369 ,  2933,  1210,  1112,  1475,  1346])
        , (35, [3363 ,  2258,   932,   856,  1135,  1036])
        , (36, [3363 ,  2258,   932,   856,  1135,  1036])
        ]
