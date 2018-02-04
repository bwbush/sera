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


module SERA.Service.HydrogenProduction.Legacy (
-- * Configuration
  ConfigProduction(..)
-- * Computation
, productionMain
) where


import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (evaluable, evaluate, knownSize)
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldCube.IO (writeFieldCubeFile)
import Data.Daft.Vinyl.FieldRec
import Data.Default.Util (inf)
import Data.Foldable (foldlM)
import Data.Map.Strict (fromListWithKey)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import GHC.Generics (Generic)
import SERA.Infrastructure.IO (InfrastructureFiles(..), readDemands)
import SERA.Infrastructure.Optimization.Legacy hiding (GlobalContext, deliveries)
import SERA.Infrastructure.Optimization.Legacy (GlobalContext(GlobalContext))
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

import qualified SERA.Infrastructure.Optimization.Legacy as G (GlobalContext(..))


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
  , maximumPathLength   :: Maybe Double
  , singleLinkPaths     :: Maybe Bool
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
    priceCube' <- readPrices priceFiles
    count "price" priceCube'
    list "Materials" $ materials priceCube'

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading upstream emission intensities " ++ show intensityFiles ++ " . . ."
    intensityCube' <- readIntensities intensityFiles
    count "intensity" intensityCube'
    list "Materials" $ materials intensityCube'
    list "Upstream materials" $ upstreamMaterials intensityCube'

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading process components and pathways . . ."
    processLibrary@ProcessLibrary{..} <- readProcessLibrary processLibraryFiles pathwayFiles
    count "cost"    processCostCube
    count "input"   processInputCube
    count "output"  processOutputCube
    count "pathway" pathwayCube
    list  "Production"           $ productions          processLibrary
    list  "Delivery"             $ deliveries           processLibrary
    list  "Transmission Pathway" $ transmissionPathways processLibrary
    list  "Local Pathway"        $ localPathways        processLibrary

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading network . . ."
    network@Network{..} <- readNetwork (fromMaybe False singleLinkPaths) (fromMaybe inf maximumPathLength) networkFiles
    count "node"      nodeCube
    count "link"      linkCube
    count "existing"  existingCube
    count "territory" territoryCube
    count "zone"      zoneCube

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Computing shortest paths . . ."
    liftIO . putStrLn $ " . . . " ++ show (length paths) ++ " paths."
--  liftIO
--    $ sequence_
--    [
--      print [show (i :: Int), show sourceId, show sinkId, show (j :: Int), show linkId, show distance]
--    |
--      (i, GenericPath{..}) <- zip [1..] $ M.elems paths
--    , (j, (linkId, distance)) <- zip [1..] linkIds
--    ]

    liftIO $ putStrLn ""
    liftIO . putStrLn $ "Reading demands " ++ show demandFiles ++ " . . ."
    demandCube' <- readDemands demandFiles
    count "demand" demandCube'

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
      priceCube = rezonePrices priceCube' zoneCube
      intensityCube = rezoneIntensities intensityCube' zoneCube
      demandCube = demandCube' ⋈ π (\_ rec -> fArea =: fArea <: rec) nodeCube :: DemandCube'
      globalContext =
        GlobalContext
          priceCube
          processLibrary
          intensityCube
          network
          (σ (\k _ -> fYear <: k <= lastYear) demandCube)
          firstYear
          lastYear
          timeWindow
          discountRate
          escalationRate
          interpolate
          (fromMaybe inf maximumPathLength)
          (fromMaybe False singleLinkPaths)
          []
          []
          []
          []

    GlobalContext _ _ _ _ _ _ _ _ _ _ _ _ _ constructions flows cashes impacts <-
      foldlM compute globalContext [firstYear, (firstYear+timeWindow) .. lastYear]

    let
      saleCube =
        fromListWithKey
          (
            \_ v1 v2 ->
                  fProduction  =: fProduction  <: v1 + fProduction  <: v2
              <+> fSale        =: fSale        <: v1 + fSale        <: v2
              <+> fConsumption =: fConsumption <: v1 + fConsumption <: v2
              <+> fSales       =: fSales       <: v1 + fSales       <: v2
          )
          $ [
              (
                    fLocation    =: fLocation <: ((fromRecords constructions :: ConstructionCube) ! (fInfrastructure =: fInfrastructure <: rec))
                <+> fYear        =: fYear <: rec
              ,     fProduction  =: fProduction <: rec
                <+> fSale        =: fSale <: rec
                <+> fConsumption =: 0
                <+> fSales       =: fSale <: rec
              )
            |
              rec <- flows
            ]
            ++
            [
              (
                    fLocation    =: fLocation <: rec
                <+> fYear        =: fYear <: rec
              ,     fProduction  =: 0
                <+> fSale        =: 0
                <+> fConsumption =: fConsumption <: rec
                <+> fSales       =: 0
              )
            |
              rec <- toKnownRecords demandCube
            , lastYear >= fYear <: rec
            ]
      saleCube' =
        κ
          (undefined :: Set (FieldRec '[FLocation]))
          (
            \_ recs ->
                  fProduction  =: sum (fmap (\rec -> fFraction <: rec * fProduction  <: rec) recs)
              <+> fSale        =: sum (fmap (\rec -> fFraction <: rec * fSale        <: rec) recs)
              <+> fCost        =: sum (fmap (\rec -> fFraction <: rec * fSale        <: rec) recs)
                                / sum (fmap (\rec -> fFraction <: rec * fProduction  <: rec) recs)
              <+> fConsumption =: sum (fmap (\rec -> fFraction <: rec * fConsumption <: rec) recs)
              <+> fSales       =: sum (fmap (\rec -> fFraction <: rec * fSales       <: rec) recs)
              <+> fNetPrice    =: sum (fmap (\rec -> fFraction <: rec * fSales       <: rec) recs)
                                / sum (fmap (\rec -> fFraction <: rec * fConsumption <: rec) recs)
          )
          $ territoryCube ⋈ saleCube

      geometryCube =
        fromRecords
          $ [
                  fLocation =: fLocation <: rec
              <+> fPosition =: Position "center"
              <+> fX        =: fX <: rec
              <+> fY        =: fY <: rec
            |
              rec <- toKnownRecords nodeCube
            ]
            ++
            [
                  fLocation =: fLocation <: rec
              <+> fPosition =: p
              <+> fX        =: fX <: (nodeCube ! n)
              <+> fY        =: fY <: (nodeCube ! n)
            |
              rec <- toKnownRecords linkCube
            , (p, n) <- zip (Position <$> ["left", "right"]) ((fLocation =:) <$> [fFrom <: rec, fTo <: rec])
            , nodeCube `evaluable` n
            ]

    writeFieldCubeFile constructionFile (fromRecords constructions :: ConstructionCube)
    writeFieldCubeFile flowFile         (fromRecords flows         :: FlowCube        )
    writeFieldCubeFile cashFile         (fromRecords cashes        :: CashCube        )
    writeFieldCubeFile impactFile       (fromRecords impacts       :: ImpactCube      )
    writeFieldCubeFile saleFile         (saleCube'                 :: SaleCube        )
    writeFieldCubeFile geometryFile     (geometryCube              :: GeometryCube    )

    liftIO $ putStrLn ""


compute :: (IsString e, MonadError e m, MonadIO m)
        => GlobalContext
        -> Year
        -> m GlobalContext
compute globalContext@GlobalContext{..} year =
  do
    let
      (supply, Optimum{..}) = optimize globalContext year
    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Years " ++ show year ++ "-" ++ show (year + timeWindow - 1) ++ " *****"
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Satisfying new demands locally . . ."
--  liftIO $ putStrLn "Identifying regional synergies . . ."
    liftIO . putStrLn $ " . . . " ++ show (length optimalConstruction) ++ " facilities constructed."
--  liftIO $ putStrLn ""
--  liftIO $ putStrLn "Searching for synergies between demand centers . . ."
--  liftIO $ putStrLn ""
--  liftIO $ putStrLn "Searching for component upgrades . . ."
    return
      globalContext
      {
        G.demandCube         = σ (\_ rec -> fConsumption <: rec > 0)
                              $ π (\key rec -> fConsumption =: maximum [0, fConsumption <: rec - maybe 0 (fConsumption <:) (supply `evaluate` τ key)] <+> fArea =: fArea <: rec) demandCube
      , G.extantConstruction = extantConstruction <> optimalConstruction
      , G.extantFlow         = extantFlow         <> optimalFlow
      , G.extantCash         = extantCash         <> optimalCash
      , G.extantImpact       = extantImpact       <> optimalImpact
      }
