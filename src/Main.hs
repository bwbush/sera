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
import Data.List (nub)
import Data.String (IsString(..))
import Data.Yaml (decodeFileEither)
import SERA (inform, stringVersion)
import SERA.Service.Finance (financeMain)
import SERA.Service.HydrogenSizing (calculateHydrogenSizing)
import SERA.Service.Introduction (calculateIntroductions)
import SERA.Service.Logistic (logisticMain)
import SERA.Service.Regionalization (calculateRegionalization)
import SERA.Service.VehicleStock (calculateStock, invertStock)
import System.Console.CmdArgs (Typeable, (&=), argPos, args, cmdArgs, def, details, help, modes, name, program, summary, typ, typFile)
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs, withArgs)
import System.Exit (die)
import System.FilePath (takeDirectory)


-- | Command-line parameters.
data SERA =
    CombineScenarios
    {
      files :: [FilePath]
    }
  | VehicleStock
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
  | HydrogenSizing
    {
      configuration :: FilePath
    } 
  | HydrogenFinance
    {
      configuration :: FilePath
    }
    deriving (Data, Show, Typeable)


-- | Command-line help.
sera :: SERA
sera =
  modes
    [
      combineScenarios
    , vehicleStock
    , invertVehicleStock
    , logistic
    , introduction
    , regionalization
    , hydrogenSizing
    , hydrogenFinance
    ]
      &= summary ("SERA command-Line, Version " ++ stringVersion ++ ", National Renewable Energy Laboratory")
      &= program "sera"
      &= help "This tool provides a command-line interface to SERA functions."


-- | Mode for combining scenarios.
combineScenarios :: SERA
combineScenarios =
  CombineScenarios
  {
    files  = def
          &= typFile
          &= args
  }
    &= name "combine-scenarios"
    &= help "Combine scenario results."
    &= details []


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
    &= help "Compute market shares using a logistic curve"
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


-- | Mode for sizing hydrogen refueling stations.
hydrogenSizing :: SERA
hydrogenSizing =
  HydrogenSizing
  {
  }
    &= name "hydrogen-station-sizing"
    &= help "Estimate capacity of hydrogen stations."
    &= details []


-- | Mode for computing refueling station finances.
hydrogenFinance :: SERA
hydrogenFinance =
  HydrogenFinance
  {
  }
    &= name "hydrogen-station-finance"
    &= help "Compute finances for hydrogen stations."
    &= details []


-- | Main action.
main :: IO ()
main =
  do
    arguments <- getArgs
    let
      arguments' -- FIXME: This is awkward.  There should be an easy way to set the default line wrapping.
        | arguments      `elem` [[], ["-?"], ["--help"]]  =             ["--help=1000"]
        | tail arguments `elem`      [["-?"], ["--help"]] = head arguments : ["--help=1000"]
        | otherwise                                  = arguments
    withArgs arguments' $ do
      command <- cmdArgs sera
      r <- runExceptT $ dispatch command
      case r :: Either String () of
        Right () -> return ()
        Left  e  -> die e


-- | Decode a YAML file.
decodeYaml :: (FromJSON a, IsString e, MonadError e m, MonadIO m)
           => FilePath -- ^ The YAML file.
           -> m a      -- ^ Action to decode the file.
decodeYaml =
  (either (throwError . fromString . displayException) return =<<)
    . liftIO
    . decodeFileEither


-- | Dispatch a computation.
dispatch :: (IsString e, MonadError e m, MonadIO m)
         => SERA -- ^ The command-line parameters.
         -> m () -- ^ Action to run SERA using the command-line parameters.
dispatch CombineScenarios{..} =
  do
    let
      trim fg@(f, g) xs
        | any null xs                  = xs
        | length (nub $ map f xs) == 1 = trim fg $ map g xs
        | otherwise                    = xs
      scenarios = trim (last, init) . trim (head, tail) $ files
    header <- liftIO $ head . lines <$> readFile (head files)
    liftIO . putStrLn $ "Scenario\t" ++ header
    sequence_
      [
        do
          contents <- liftIO $ tail . lines <$> readFile file
          mapM_ (liftIO . putStrLn . ((scenario ++ "\t") ++)) contents
      |
        (scenario, file) <- zip scenarios files
      ]
dispatch s@VehicleStock{}       = dispatch' calculateStock           s
dispatch s@InvertVehicleStock{} = dispatch' invertStock              s
dispatch s@Logistic{}           = dispatch' logisticMain             s
dispatch s@Introduction{}       = dispatch' calculateIntroductions   s
dispatch s@Regionalization{}    = dispatch' calculateRegionalization s
dispatch s@HydrogenSizing{}     = dispatch' calculateHydrogenSizing  s
dispatch s@HydrogenFinance{}    = dispatch' financeMain              s


-- | Help dispatch an operation.
dispatch' :: (IsString e, MonadError e m, MonadIO m, FromJSON a)
          => (a -> m ()) -- ^ Operation to perform.
          -> SERA        -- ^ Command-line parameters.
          -> m ()        -- ^ Action to perform the operation using the command-line parameters.
dispatch' operation s =
  do
    let
      path = configuration s
    configuration' <- decodeYaml path
    inform $ "Setting working directory to \"" ++ (takeDirectory path) ++ "\""
    liftIO . setCurrentDirectory $ takeDirectory path
    operation configuration'
