-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.HydrogenSizing
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for computing hydrogen statiomn sizes for a scenario.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeOperators   #-}


module SERA.Service.HydrogenProduction (
-- * Configuration
  ConfigProduction(..)
-- * Computation
, productionMain
) where


import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (knownSize)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA.Infrastructure.IO (InfrastructureFiles(..), readDemands)
import SERA.Material.IO (readIntensities, readPrices)
import SERA.Material.Types -- FIXME
import SERA.Network.IO (NetworkFiles(..), readNetwork)
import SERA.Network.Types -- FIXME
import SERA.Process.IO (ProcessLibraryFiles, readProcessLibrary)
import SERA.Process.Types -- FIXME
import SERA.Service ()
import SERA.Types (Year)


-- | Configuration for hydrogen station sizing.
data ConfigProduction =
  ConfigProduction
  {
    firstYear           :: Year
  , lastYear            :: Year
  , timeWindow          :: Year
  , discountRate        :: Double
  , escalationRate      :: Double
  , interpolate         :: Bool
  , priceFiles          :: [FilePath]
  , intensityFiles      :: [FilePath]
  , processLibraryFiles :: [ProcessLibraryFiles]
  , pathwayFiles        :: [FilePath]
  , networkFiles        :: NetworkFiles
  , demandFiles         :: [FilePath]
  , infrastructureFiles :: InfrastructureFiles
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigProduction

instance ToJSON ConfigProduction


-- | Compute hydrogen station sizes.
productionMain :: (IsString e, MonadError e m, MonadIO m)
                       => ConfigProduction -- ^ Configuration data.
                       -> m ()                 -- ^ Action to compute the station sizes.
productionMain ConfigProduction{..} =

  do
    let
      count label content =
        liftIO
          . putStrLn
          $ " . . . " ++ show (knownSize content) ++ (if null label then "" else ' ' : label) ++ " records."
      list label content =
        liftIO
          $ do
            putStrLn $ label ++ ":"
            mapM_ (putStrLn . ("  " ++) . show) content

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading price files " ++ show priceFiles ++ " . . ."
    priceCube <- readPrices priceFiles
    count "price" priceCube
    list "Materials" $ materials priceCube

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading upstream emission intensities " ++ show intensityFiles ++ " . . ."
    intensityCube <- readIntensities intensityFiles
    count "intensity" intensityCube
    list "Materials" $ materials intensityCube
    list "Upstream materials" $ upstreamMaterials intensityCube

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading process components and pathways . . ."
    processLibrary@ProcessLibrary{..} <- readProcessLibrary processLibraryFiles pathwayFiles
    count "cost"    processCostCube
    count "input"   processInputCube
    count "output"  processOutputCube
    count "pathway" pathwayCube
    list  "Production"  $ productions processLibrary
    list  "Delivery"    $ deliveries  processLibrary
    list  "Pathway"     $ pathways    processLibrary

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading network . . ."
    Network{..} <- readNetwork networkFiles
    count "node"      nodeCube
    count "link"      linkCube
    count "existing"  existingCube
    count "territory" territoryCube
    count "zone"      zoneCube

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading demands " ++ show demandFiles ++ " . . ."
    demandCube <- readDemands demandFiles
    count "demand" demandCube

    liftIO
      $ do
        putStrLn ""
        putStrLn "Optimization parameters:"
        putStrLn $ "  First Year:            " ++ show firstYear
        putStrLn $ "  Last Year:             " ++ show lastYear
        putStrLn $ "  Time Window:           " ++ show timeWindow
        putStrLn $ "  Discount Rate [/yr]:   " ++ show discountRate
        putStrLn $ "  Escalation Rate [/yr]: " ++ show escalationRate
        putStrLn $ "  Interpolate?           " ++ show interpolate

    let
      InfrastructureFiles{..} = infrastructureFiles

    sequence_
      [
        do
          liftIO $ putStrLn ""
          liftIO $ putStrLn ""
          liftIO . putStrLn $ "***** Years " ++ show year ++ "-" ++ show (year + timeWindow - 1) ++ " *****"
          liftIO $ putStrLn ""
          liftIO $ putStrLn "Satisfying new demands locally . . ."
          liftIO $ putStrLn ""
          liftIO $ putStrLn "Searching for synergies between demand centers . . ."
          liftIO $ putStrLn ""
          liftIO $ putStrLn "Searching for component upgrades . . ."
      |
        year <- take 1 [firstYear, (firstYear+timeWindow) .. lastYear] :: [Year]
      ]

    liftIO $ putStrLn ""
