{-# LANGUAGE RecordWildCards #-}


-- | Module for accessing energy prices.
module AEO.Prices.Data {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  loadPrices
) where


import AEO.Prices (AeoPrices(..), Price, PriceKey)
import AEO.Types (Region(..), Sector(..), Source(..))
import Data.Maybe (fromJust)
import SERA.Util.Time (Year(..))

import qualified AEO.Bulk as A
import qualified Data.Map as M


nominalPrices :: Bool
nominalPrices = True


loadPrices :: Int -> String -> IO AeoPrices
loadPrices year caseLabel =
  do
    let
      idXref' = idXref year caseLabel
      ids = map fst idXref'
    bulkData <- filter ((`elem` ids) . A.aeoId) <$> A.getBulkAeo 2014
    return 
      AeoPrices
        {
          aeoDateKey = year
        , aeoCase    = caseLabel
        , aeoUnits   = if nominalPrices then "nom $/mill Btu" else "2012 %/mill Btu"
        , aeoPrices  = M.fromList $ concatMap (expandPrices idXref') bulkData
        }


expandPrices :: [(String, (Region, Sector, Source))] -> A.AeoSeries -> [(PriceKey, Price)]
expandPrices idsXref' A.AeoSeries{..} =
  let
    Just (region, sector, source) = lookup aeoId idsXref'
  in
    [
      ((region, sector, source, Year year), value)
    |
      (year, value) <- aeoData
    ]


idXref :: Int -> String -> [(String, (Region, Sector, Source))]
idXref year caseLabel =
  [
    (makeId year caseLabel region sector source field unit, (fromJust $ lookup region regionXref, fromJust $ lookup sector sectorXref, fromJust $ lookup source sourceXref))
  |
    region <- map fst regionXref
  , sector <- map fst sectorXref
  , source <- map fst sourceXref
  , field <- if nominalPrices then ["PRCE_NOM"] else ["PRCE_APTU", "PRCE_ENE"]
  , unit  <- if nominalPrices then ["NDLRPMBTU"] else ["Y12DLRPMMBTU"]
  ]


makeId :: Int -> String -> String -> String -> String -> String -> String -> String
makeId year caseLabel region sector source field unit =
  "AEO."
    ++ show year
    ++ "."
    ++ caseLabel
    ++ "."
    ++ field
    ++ "_"
    ++ sector
    ++ "_NA_"
    ++ source
    ++ "_NA_"
    ++ region
    ++ "_"
    ++ unit
    ++ ".A"


regionXref :: [(String, Region)]
regionXref =
  [
    ("NA"    , UnitedStates    )
  , ("NEENGL", NewEngland      )
  , ("MDATL" , MiddleAtlantic  )
  , ("ENC"   , EastNorthCentral)
  , ("WNC"   , WestNorthCentral)
  , ("SOATL" , SouthAtlantic   )
  , ("ENC"   , EastNorthCentral)
  , ("WNC"   , WestNorthCentral)
  , ("ESC"   , EastSouthCentral)
  , ("WSC"   , WestSouthCentral)
  , ("MTN"   , Mountain        )
  , ("PCF"   , Pacific         )
  ]


sectorXref :: [(String, Sector)]
sectorXref =
  [
    ("RES" , Residential   )
  , ("CMM" , Commercial    )
  , ("IDAL", Industrial    )
  , ("TRN" , Transportation)
  , ("ELEP", ElectricPower )
  , ("TEN" , AllUses       )
  ]


sourceXref :: [(String, Source)]
sourceXref =
  [
    ("PROP" , Propane            )
  , ("DFO"  , DistillateFuelOil  )
  , ("DFU"  , DistillateFuelOil  )
  , ("RFL"  , ResidualFuelOil    )
  , ("RFO"  , ResidualFuelOil    )
  , ("NG"   , NaturalGas         )
  , ("ELC"  , Electricity        )
  , ("MTC"  , MetallurgicalCoal  )
  , ("OIC"  , OtherIndustrialCoal)
  , ("OCA"  , OtherIndustrialCoal)
  , ("CLTLQ", CoalToLiquids      )
  , ("E85"  , E85                )
  , ("MGS"  , MotorGasoline      )
  , ("JFL"  , JetFuel            )
  , ("STC"  , SteamCoal          )
  ]
