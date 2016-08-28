{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module VISION.Survival (
  survivalFunction
) where


import Data.Daft.DataCube (fromFunction)
import Data.Daft.Vinyl.FieldCube (type (↝))
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Vinyl.Derived ((=:))
import SERA.Types (FYear, fYear)
import SERA.Vehicle.Types (FClassification, FModelYear, FSurvival, Survival, fModelYear, fSurvival)

import Data.Map.Strict (Map, fromList, lookupLE)


survivalFunction :: '[FClassification, FModelYear, FYear] ↝ '[FSurvival]
survivalFunction =
  fromFunction $ \rec ->
     ((fSurvival =:) . snd)
       <$> (fYear <: rec - fModelYear <: rec) `lookupLE` survivalTable


survivalTable :: Map Int Survival
survivalTable =
  fromList
    $ zip [0..]
    [
      1
    , 0.99
    , 0.9801
    , 0.970299
    , 0.96059601
    , 0.95099005
    , 0.941480149
    , 0.932065348
    , 0.922744694
    , 0.895062354
    , 0.868210483
    , 0.807435749
    , 0.750915247
    , 0.698351179
    , 0.628516062
    , 0.540523813
    , 0.464850479
    , 0.399771412
    , 0.343803414
    , 0.295670936
    , 0.242450168
    , 0.198809138
    , 0.163023493
    , 0.133679264
    , 0.109616997
    , 0.089885937
    , 0.073706468
    , 0.060439304
    , 0.049560229
    , 0.040639388
    , 0.033324298
    , 0.027325925
    , 0.022407258
    , 0.018373952
    , 0.01506664
    , 0.012354645
    , 0.010130809
    , 0.008307263
    , 0.006811956
    , 0.005585804
    , 0.004580359
    , 0.003755895
    , 0.003079834
    , 0.002525463
    , 0.00207088
    , 0.001698122
    , 0.00139246
    , 0.001141817
    , 0.00093629
    , 0.000767758
    , 0.000629561
    , 0.00051624
    , 0.000423317
    , 0.00034712
    , 0.000284638
    , 0.000233403
    , 0.000191391
    , 0.00015694
    , 0.000128691
    , 0.000105527
    , 0.000086532
    , 7.09562E-005
    , 5.81841E-005
    , 0.000047711
    , 0.000039123
    , 3.20808E-005
    , 2.63063E-005
    , 2.15712E-005
    , 1.76884E-005
    , 1.45044E-005
    , 1.18936E-005
    ]
