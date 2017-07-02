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
import Data.Daft.Vinyl.FieldRec ((=:), (<+>))
import Data.Daft.Vinyl.FieldRec.IO (showFieldRecs)
import Data.List.Util.Listable (toTabbeds)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA.Infrastructure.IO (InfrastructureFiles(..), readDemands)
import SERA.Infrastructure.Types (Infrastructure(..), fInfrastructure)
import SERA.Material.IO (readIntensities, readPrices)
import SERA.Material (makePricer)
import SERA.Material.Prices (materials)
import SERA.Network.IO (NetworkFiles(..), readNetwork)
import SERA.Network.Types (fFrom, Location(..), Network(..), fTo, Zone(..), fZone)
import SERA.Process (deliveries, pathways, productions)
import SERA.Process.IO (ProcessLibraryFiles, readProcessLibrary)
import SERA.Process.Reification.Pathway (deliveryReifier, transmissionReifier)
import SERA.Process.Reification.Technology (technologyReifier)
import SERA.Process.Types (Pathway(..), Technology(..))
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
    priceCube <- readPrices priceFiles
    intensityCube <- readIntensities intensityFiles
    processLibrary <- readProcessLibrary processLibraryFiles pathwayFiles
    network <- readNetwork networkFiles
    demandCube <- readDemands demandFiles
    liftIO
      $ do
        let
          list label content =
            do
              putStrLn ""
              putStrLn $ label ++ ":"
              mapM_ (putStrLn . ("  " ++) . show) content
          count label content =
            do
              putStrLn ""
              putStrLn $ label ++ ": " ++ show (knownSize content) ++ " keys"
          InfrastructureFiles{..} = infrastructureFiles
        putStrLn ""
        putStrLn $ "First Year:            " ++ show firstYear
        putStrLn $ "Last Year:             " ++ show lastYear
        putStrLn $ "Time Window:           " ++ show timeWindow
        putStrLn $ "Discount Rate [/yr]:   " ++ show discountRate
        putStrLn $ "Escalation Rate [/yr]: " ++ show escalationRate
        putStrLn $ "Interpolate?           " ++ show interpolate
        list  "Material"    $ materials     priceCube
        count "Intensities"                 intensityCube
        list  "Production"  $ productions   processLibrary
        list  "Delivery"    $ deliveries    processLibrary
        list  "Pathway"     $ pathways      processLibrary
        count "Nodes"       $ nodeCube      network
        count "Links"       $ linkCube      network
        count "Existings"   $ existingCube  network
        count "Territories" $ territoryCube network
        count "Zones"       $ zoneCube      network
        count "Demands"                     demandCube
        putStrLn ""
        putStrLn $ "Construction: " ++ constructionFile
        putStrLn $ "Flow:         " ++ flowFile
        putStrLn $ "Cash:         " ++ cashFile
        putStrLn $ "Impact:       " ++ impactFile
        putStrLn $ "Sale:         " ++ saleFile
        putStrLn ""
        let
          specifics = fInfrastructure =: Infrastructure "Test" <+> fFrom =: Location "Source" <+> fTo =: Location "Sink"
          reifyTechnology =
            technologyReifier
              processLibrary
              (makePricer priceCube $ fZone =: Zone "Mountain")
          Just (c, f) =
            reifyTechnology
              specifics
              (Technology "Central Natural Gas Reforming")
              2030
              2000000000
              0
          (fl, ca, im) = f 2030 100
          reifyTransmission =
            transmissionReifier
              processLibrary
              reifyTechnology
          Just (c', f') =
            reifyTransmission
              specifics
              (Pathway "LH2 Rail")
              2030
              2000000000
              500
          (fl', ca', im') = f' 2030 100
          reifyDelivery =
            deliveryReifier
              processLibrary
              reifyTechnology
          Just (c'', f'') =
            reifyDelivery
              specifics
              (Pathway "LH2 Rail")
              2030
              2000000000
              50
          (fl'', ca'', im'') = f'' 2030 100
        putStrLn . toTabbeds $ showFieldRecs [c]
        putStrLn . toTabbeds $ showFieldRecs [fl]
        putStrLn . toTabbeds $ showFieldRecs ca
        putStrLn . toTabbeds $ showFieldRecs im
        putStrLn . toTabbeds $ showFieldRecs c'
        putStrLn . toTabbeds $ showFieldRecs fl'
        putStrLn . toTabbeds $ showFieldRecs ca'
        putStrLn . toTabbeds $ showFieldRecs im'
        putStrLn . toTabbeds $ showFieldRecs c''
        putStrLn . toTabbeds $ showFieldRecs fl''
        putStrLn . toTabbeds $ showFieldRecs ca''
        putStrLn . toTabbeds $ showFieldRecs im''
