-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Command-line tool for SERA.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-missing-fields #-}


module Main (
-- * Entry point
  main
) where


import Control.Exception.Base (displayException)
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExceptT, throwError)
import Data.Aeson.Types (FromJSON)
import Data.Data (Data)
import Data.String (IsString(..))
import Data.Yaml (decodeFileEither)
import SERA (inform, stringVersion)
import SERA.Service.Introduction (calculateIntroductions)
import SERA.Service.Logistic (calculateLogistic)
import SERA.Service.Regionalization (calculateRegionalization)
import SERA.Service.VehicleStock (calculateStock, invertStock)
import System.Console.CmdArgs (Typeable, (&=), argPos, cmdArgs, def, details, help, modes, name, program, summary, typ)
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs, withArgs)
import System.FilePath (takeDirectory)


-- | Command-line parameters.
data SERA =
    VehicleStock
    {
      configuration :: FilePath
    } 
  | InvertVehicleStock
    {
      configuration :: FilePath
    } 
  | Logistic
    {
      configuration :: FilePath
    } 
  | Introduction
    {
      configuration :: FilePath
    } 
  | Regionalization
    {
      configuration :: FilePath
    } 
    deriving (Data, Show, Typeable)


-- | Command-line help.
sera :: SERA
sera =
  modes
    [
      vehicleStock
    , invertVehicleStock
    , logistic
    , introduction
    , regionalization
    ]
      &= summary ("SERA command-Line, Version " ++ stringVersion ++ ", National Renewable Energy Laboratory")
      &= program "sera"
      &= help "This tool provides a command-line interface to SERA functions."


-- | Mode for computing vehicle stock.
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


-- | Mode for inverting a vehicle stock computation.
invertVehicleStock :: SERA
invertVehicleStock =
  InvertVehicleStock
  {
  }
    &= name "invert-stock"
    &= help "Invert a table of vehicle stock, computing sales from stock."
    &= details []


-- | Mode for applying logistic curves.
logistic :: SERA
logistic =
  Logistic
  {
  }
    &= name "logistic-scenario"
    &= help "Apply a logistic curve to vehicle sales."
    &= details []


-- | Mode for estimating introduction years.
introduction :: SERA
introduction =
  Introduction
  {
  }
    &= name "introduction-year"
    &= help "Estimate introduction years for new vehicles."
    &= details []


-- | Mode for regionalizing demand.
regionalization :: SERA
regionalization =
  Regionalization
  {
  }
    &= name "regionalization"
    &= help "Regionalize demand for new vehicles."
    &= details []


-- | Main action.
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


-- | Decode a YAML file.
decodeYaml :: (FromJSON a, IsString e, MonadError e m, MonadIO m) => FilePath -> m a
decodeYaml =
  (either (throwError . fromString . displayException) return =<<)
    . liftIO
    . decodeFileEither


-- | Dispatch a computation.

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
    inform $ "Setting working directory to \"" ++ (takeDirectory configuration) ++ "\""
    liftIO . setCurrentDirectory $ takeDirectory configuration
    invertStock configuration'

dispatch Logistic{..} =
  do
    configuration' <- decodeYaml configuration
    inform $ "Setting working directory to \"" ++ (takeDirectory configuration) ++ "\""
    liftIO . setCurrentDirectory $ takeDirectory configuration
    calculateLogistic configuration'

dispatch Introduction{..} =
  do
    configuration' <- decodeYaml configuration
    inform $ "Setting working directory to \"" ++ (takeDirectory configuration) ++ "\""
    liftIO . setCurrentDirectory $ takeDirectory configuration
    calculateIntroductions configuration'

dispatch Regionalization{..} =
  do
    configuration' <- decodeYaml configuration
    inform $ "Setting working directory to \"" ++ (takeDirectory configuration) ++ "\""
    liftIO . setCurrentDirectory $ takeDirectory configuration
    calculateRegionalization configuration'
