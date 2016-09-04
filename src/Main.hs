{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-missing-fields #-}


module Main (
  main
) where


import Control.Exception.Base (displayException)
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExceptT, throwError)
import Data.Aeson.Types (FromJSON)
import Data.Data (Data)
import Data.String (IsString(..))
import Data.Yaml (decodeFileEither)
import SERA (inform, stringVersion)
import SERA.Service.VehicleStock (calculateStock, invertStock)
import System.Console.CmdArgs (Typeable, (&=), argPos, cmdArgs, def, details, help, modes, name, program, summary, typ)
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs, withArgs)
import System.FilePath (takeDirectory)


data SERA =
    VehicleStock
    {
      configuration :: FilePath
    } 
  | InvertVehicleStock
    {
      configuration :: FilePath
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
  {
    configuration  = def
                  &= typ "YAML_CONFIGURATION"
                  &= argPos 1
  }
    &= name "stock"
    &= help "Compute vehicle stock."
    &= details []


invertVehicleStock :: SERA
invertVehicleStock =
  InvertVehicleStock
  {
  }
    &= name "invert-stock"
    &= help "Invert a table of vehicle stock, computing sales from stock."
    &= details
       [
         "The format of the YAML_CONFIGURATION file is as follows:"
       , ""
       , "    Field                             Description                                                     "
       , "    --------------------------------  ----------------------------------------------------------------"
       , "    stockFile     : STOCK_FILE        # name of the input vehicle stock file"
       , "    salesStockFile: SALES_STOCK_FILE  # name of the output vehicle sales file"
       , "    survival      : FILE filename     # name of survival function (default = VISION)"
       , "    priorYears    : INTEGER           # number of prior years for which to compute sales (default = 0)"
       , ""
       , "The STOCK_FILE must be in tab-separate-value format with the following columns:"
       , ""
       , "    Name            Type     Description                  "
       , "    --------------  -------  -----------------------------"
       , "    Region          text     geographic region            "
       , "    Classification  text     vehicle class and/or vocation"
       , "    Year            integer  calendar year                "
       , "    Stock [veh]     real     total number of vehicles     "
       , ""
       , "The SALES_STOCK_FILE will be in tab-separate-value format with the following columns:"
       , ""
       , "    Name            Type     Description                  "
       , "    --------------  -------  -----------------------------"
       , "    Region          text     geographic region            "
       , "    Classification  text     vehicle class and/or vocation"
       , "    Year            integer  calendar year                "
       , "    Sales [veh]     real     number of vehicles sold      "
       , "    Stock [veh]     real     total number of vehicles     "
       ]


main :: IO ()
main =
  do
    args <- getArgs
    let
      args' -- FIXME: This is awkward.  There should be an easy way to set the default line wrapping.
        | args      `elem` [[], ["-?"], ["--help"]]  =             ["--help=1000"]
        | tail args `elem`      [["-?"], ["--help"]] = head args : ["--help=1000"]
        | otherwise                                  = args
    withArgs args' $ do
      command <- cmdArgs sera
      r <- runExceptT $ dispatch command
      case r :: Either String () of
        Right () -> return ()
        Left  e  -> putStrLn e


decodeYaml :: (FromJSON a, IsString e, MonadError e m, MonadIO m) => FilePath -> m a
decodeYaml =
  (either (throwError . fromString . displayException) return =<<)
    . liftIO
    . decodeFileEither


dispatch :: (IsString e, MonadError e m, MonadIO m) => SERA -> m ()

dispatch VehicleStock{..} =
  do
    configuration' <- decodeYaml configuration
    inform $ "Setting working directory to \"" ++ (takeDirectory configuration) ++ "\""
    liftIO . setCurrentDirectory $ takeDirectory configuration
    calculateStock configuration'

dispatch InvertVehicleStock{..} =
  do
    configuration' <- decodeYaml configuration
    invertStock configuration'
