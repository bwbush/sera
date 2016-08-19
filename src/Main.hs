{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}


module Main (
  main
) where


import Control.Monad.Except (MonadError, MonadIO, runExceptT)
import Data.Data (Data)
import Data.Daft.Vinyl.FieldRec (readFieldRecFile, writeFieldRecFile)
import Data.String (IsString)
import SERA (stringVersion)
import SERA.Vehicle.Stock (inferSales)
import System.Console.CmdArgs (Typeable, (&=), argPos, auto, cmdArgs, def, details, explicit, help, modes, name, program, summary, typ)
import VISION.Survival (survivalFunction)


data SERA =
    VehicleStock
  | InvertVehicleStock
    {
      priorYears     :: Int
    , stockFile      :: FilePath
    , salesStockFile :: FilePath
    } 
    deriving (Data, Show, Typeable)


sera :: SERA
sera =
  modes
    [
      vehicleStock
    , invertVehicleStock
    ]
      &= summary ("SERA command-Line, Version " ++ stringVersion ++ ", National Renewable Energy Laboratory")
      &= program "sera"
      &= help "This tool provides a command-line interface to SERA functions."


vehicleStock :: SERA
vehicleStock =
  VehicleStock
    &= name "stock"
    &= help "Model vehicle stock."
    &= details []
    &= auto


invertVehicleStock :: SERA
invertVehicleStock =
  InvertVehicleStock
  {
    priorYears      = def
                   &= typ "INTEGER"
                   &= explicit
                   &= name "prior-years"
                   &= help "number of prior years for which to compute vehicle sales"
  , stockFile       = def
                   &= argPos 0
                   &= typ "VEHICLE_STOCK"
  , salesStockFile  = def
                   &= argPos 1
                   &= typ "VEHICLE_SALES_AND_STOCK"
  }
    &= name "invert-stock"
    &= help "Invert a table of vehicle stock."
    &= details []


main :: IO ()
main =
  do
    command <- cmdArgs sera
    r <- runExceptT $ dispatch command
    case r :: Either String () of
      Right () -> return ()
      Left  e  -> print e


dispatch :: (IsString e, MonadError e m, MonadIO m) => SERA -> m ()

dispatch VehicleStock =
  undefined

dispatch InvertVehicleStock{..} =
  do
    stock <- readFieldRecFile stockFile
    let
      sales = inferSales priorYears survivalFunction stock
    writeFieldRecFile salesStockFile sales
