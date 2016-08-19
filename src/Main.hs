module Main (
  main
) where


import Data.Daft.Vinyl.FieldRec (showFieldRecs)
import Data.List (intercalate)
import Data.Vinyl.Core ((<+>))
import Data.Vinyl.Derived ((=:))
import SERA.Types (Region(..), fRegion, fYear)
import SERA.Vehicle.Stock (inferSales)
import SERA.Vehicle.Stock.Types (StockRecord)
import SERA.Vehicle.Types (Classification(..), fClassification, fStock)
import VISION.Survival (survivalFunction)


x :: [StockRecord]
x =
  zipWith (\y s -> fRegion =: Region "USA" <+> fClassification =: Classification "any" <+> fYear =: y <+> fStock =: s)
    [2010..]
    [22.1555407001201, 22.010524228269 , 21.7213560306234, 20.8349776256459, 19.8573776272418, 18.9068264081536, 17.9529979653946, 17.0707808818835, 16.2819975572411, 15.5121073775302, 14.7416389188591, 13.9791006475468, 13.2354947328751, 12.5056217085559, 11.7966417913567, 11.1113810993403, 10.4674659201071, 9.86070554258323, 9.29177769650786, 8.76105480010271, 8.26055435774512, 7.79923925852572, 7.40133373252114, 7.04298767318795, 6.70205189549499, 6.42690845264281, 6.16102287149246, 5.92275001283085, 5.70703730944142, 5.51861943048115, 5.34721355531326, 5.1873873669748 , 5.04348741570672, 4.94599741616497, 4.81281777901556, 4.69577713769456, 4.57281268409576, 4.47383254564273, 4.37777553440783, 4.29912939369518, 4.26079979774038]


main :: IO ()
main =
  putStrLn
    . unlines
    . map (intercalate "\t")
    . showFieldRecs
    $ inferSales 30 survivalFunction x
