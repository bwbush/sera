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


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Service.HydrogenProduction (
-- * Configuration
  ConfigProduction(..)
-- * Computation
, productionMain
) where


import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (evaluate, knownSize)
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldRec
import Data.Daft.Vinyl.FieldRec.IO (writeFieldRecFile)
import Data.Foldable (foldlM)
import Data.List (unzip4)
import Data.Set (Set)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import GHC.Generics (Generic)
import SERA.Infrastructure.IO (InfrastructureFiles(..), readDemands)
import SERA.Infrastructure.Optimization
import SERA.Infrastructure.Types 
import SERA.Material.IO (readIntensities, readPrices)
import SERA.Material.Prices
import SERA.Material.Types -- FIXME
import SERA.Network.IO (NetworkFiles(..), readNetwork)
import SERA.Network.Types -- FIXME
import SERA.Process.IO (ProcessLibraryFiles, readProcessLibrary)
import SERA.Process.Types -- FIXME
import SERA.Service ()
import SERA.Types


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
      priceCube' = rezonePrices priceCube zoneCube
      demandCube' = demandCube ⋈ π (\_ rec -> fArea =: fArea <: rec) nodeCube :: DemandCube'

    (_, (constructions, flows, cashes, impacts)) <-
      foldlM
        (compute priceCube' processLibrary timeWindow)
        (demandCube', ([], [], [], []))
        [firstYear, (firstYear+timeWindow) .. lastYear]

    writeFieldRecFile constructionFile constructions
    writeFieldRecFile flowFile         flows
    writeFieldRecFile cashFile         cashes
    writeFieldRecFile impactFile       impacts

    liftIO $ putStrLn ""


type DemandCube' = '[FLocation, FYear] *↝ '[FConsumption, FArea]


compute :: (IsString e, MonadError e m, MonadIO m)
        => PriceCube '[FLocation]
        -> ProcessLibrary
        -> Year
        -> (DemandCube', ([Construction], [Flow], [Cash], [Impact]))
        -> Year
        -> m (DemandCube', ([Construction], [Flow], [Cash], [Impact]))
compute priceCube processLibrary timeWindow (demandCube, (constructions, flows, cashes, impacts)) year =
  do
    let
      allYears :: Set (FieldRec '[FYear])
      allYears = undefined
      filterYear :: (FYear ∈ ks) => FieldRec ks -> Bool
      filterYear rec = fYear <: rec >= year && fYear <: rec < year + timeWindow
      results :: '[FLocation] *↝ '[FYear, FConsumption, FOptimum]
      results =
          κ' allYears (cheapestLocally priceCube processLibrary)
        $ σ (const . filterYear) demandCube
      (constructions', flows', cashes', impacts') = unzip4 $ fmap (fOptimum <:) $ toKnownRecords results
    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Years " ++ show year ++ "-" ++ show (year + timeWindow - 1) ++ " *****"
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Satisfying new demands locally . . ."
    liftIO . putStrLn $ " . . . " ++ show (length constructions') ++ " facilities constructed."
--  liftIO $ putStrLn ""
--  liftIO $ putStrLn "Searching for synergies between demand centers . . ."
--  liftIO $ putStrLn ""
--  liftIO $ putStrLn "Searching for component upgrades . . ."
    return
      (
        π (\key rec -> fConsumption =: maximum [0, fConsumption <: rec - maybe 0 (fConsumption <:) (results `evaluate` τ key)] <+> fArea =: fArea <: rec) demandCube
      , (
          constructions ++ constructions'
        , flows         ++ concat flows'
        , cashes        ++ concat cashes'
        , impacts       ++ concat impacts'
        )
      )
