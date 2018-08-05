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


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}


module SERA.Service.HydrogenProduction (
-- * Configuration
  ConfigProduction(..)
-- * Computation
, productionMain
) where


import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logCritical, logInfo)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (evaluable, evaluate, knownSize)
import Data.Daft.Vinyl.FieldCube
import Data.Daft.Vinyl.FieldCube.IO (writeFieldCubeFile)
import Data.Daft.Vinyl.FieldRec
import Data.Default.Util (inf)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Map.Strict (fromListWithKey)
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import GHC.Generics (Generic)
import SERA (SeraLog)
import SERA.Demand (checkDemands, readDemands, readPeriods)
import SERA.Infrastructure (InfrastructureFiles(..))
import SERA.Infrastructure.Optimization (Optimum(..), Strategy(..), optimize)
import SERA.Material (checkIntensities, checkPrices, materials, readIntensities, readPrices, rezoneIntensities, rezonePrices, upstreamMaterials)
import SERA.Network (Network(..), NetworkFiles(..), checkNetwork, readNetwork)
import SERA.Process (ProcessLibrary(..), ProcessLibraryFiles, checkProcessLibrary, deliveries, localPathways, productions, readProcessLibrary, transmissionPathways)
import SERA.Service ()
import SERA.Types.Cubes
import SERA.Types.Fields


-- | Configuration for hydrogen station sizing.
data ConfigProduction =
  ConfigProduction
  {
    firstYear           :: Year
  , lastYear            :: Year
  , timeWindow          :: Year
  , strategy            :: Maybe Strategy
  , discountRate        :: Maybe Double
  , escalationRate      :: Maybe Double
  , interpolate         :: Maybe Bool
  , maximumPathLength   :: Maybe Double
  , singleLinkPaths     :: Maybe Bool
  , priceFiles          :: [FilePath]
  , intensityFiles      :: [FilePath]
  , processLibraryFiles :: [ProcessLibraryFiles]
  , pathwayFiles        :: [FilePath]
  , networkFiles        :: NetworkFiles
  , demandFiles         :: [FilePath]
  , periodFiles         :: [FilePath]
  , infrastructureFiles :: InfrastructureFiles
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigProduction

instance ToJSON ConfigProduction


-- | Compute hydrogen station sizes.
productionMain :: (IsString e, MonadError e m, MonadIO m, SeraLog m)
                       => ConfigProduction -- ^ Configuration data.
                       -> m ()                 -- ^ Action to compute the station sizes.
productionMain ConfigProduction{..} =

  do

    logInfo ""
    periodCube <- readPeriods periodFiles

    logInfo ""
    priceCube' <- readPrices priceFiles

    logInfo ""
    intensityCube' <- readIntensities intensityFiles

    logInfo ""
    processLibrary@ProcessLibrary{..} <- readProcessLibrary processLibraryFiles pathwayFiles
    logInfo ""
    network@Network{..} <- readNetwork (fromMaybe False singleLinkPaths) (fromMaybe inf maximumPathLength) networkFiles

    logInfo ""
    demandCube' <- readDemands True demandFiles

    logInfo ""
    checkNetwork network
    checkProcessLibrary processLibrary existingCube
    checkDemands nodeCube demandCube'
    checkPrices network processLibrary intensityCube' priceCube'
    checkIntensities network processLibrary intensityCube'
    logInfo ". . . checks complete."

    logInfo ""
    let
      list label content =
        do
          logInfo $ label ++ ":"
          mapM_ (logInfo . ("  " ++) . show) content
    list "Production"           $ productions          processLibrary
    list "Delivery"             $ deliveries           processLibrary
    list "Transmission Pathway" $ transmissionPathways processLibrary
    list "Local Pathway"        $ localPathways        processLibrary
    list "Input materials"      $ materials            processInputCube
    list "Output materials"     $ materials            processOutputCube
    list "Upstream materials"   $ upstreamMaterials    intensityCube'
    list "Priced materials"     $ materials            priceCube'

    let
      strategy'       = fromMaybe LiteralInWindow strategy
      discountRate'   = fromMaybe 0               discountRate
      escalationRate' = fromMaybe 0               escalationRate
      interpolate'    = fromMaybe True            interpolate

    logInfo ""
    logInfo "Optimization parameters:"
    logInfo $ "  First Year:            " ++ show firstYear
    logInfo $ "  Last Year:             " ++ show lastYear
    logInfo $ "  Time Window:           " ++ show timeWindow
    logInfo $ "  Strategy:              " ++ show strategy'
    logInfo $ "  Discount Rate [/yr]:   " ++ show discountRate'
    logInfo $ "  Escalation Rate [/yr]: " ++ show escalationRate'
    logInfo $ "  Interpolate?           " ++ show interpolate'

    let

      InfrastructureFiles{..} = infrastructureFiles
      priceCube = rezonePrices priceCube' zoneCube
      intensityCube = rezoneIntensities intensityCube' zoneCube
      demandCube = demandCube' ⋈ π (\_ rec -> fArea =: fArea <: rec) nodeCube :: DemandAreaCube

    (failure, Optimum constructions flows cashes impacts) <-
      optimize
        (chunksOf timeWindow [firstYear..lastYear])
        periodCube
        network
        demandCube'
        intensityCube
        processLibrary
        priceCube
        discountRate'
        escalationRate'
        strategy'

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
                    fLocation    =: fLocation <: ((fromRecords constructions' :: ConstructionCube) ! (fInfrastructure =: fInfrastructure <: rec))
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
                <+> fConsumption =: fFuelConsumption <: rec + fNonFuelConsumption <: rec
                <+> fSales       =: 0
              )
            |
              rec <- toKnownRecords demandCube
            , firstYear <= fYear <: rec
            , lastYear  >= fYear <: rec
            ]
      saleCube' =
        κ'
          (undefined :: Set (FieldRec '[FLocation]))
          (
            \recs ->
                  fProduction  =: sum (fmap (\rec -> fFraction <: rec * fProduction  <: rec) recs)
              <+> fSale        =: sum (fmap (\rec -> fFraction <: rec * fSale        <: rec) recs)
              <+> fCost        =: sum (fmap (\rec -> fFraction <: rec * fSale        <: rec) recs)
                                / sum (fmap (\rec -> fFraction <: rec * fProduction  <: rec) recs)
              <+> fConsumption =: sum (fmap (\rec -> fFraction <: rec * fConsumption <: rec) recs)
              <+> fSales       =: sum (fmap (\rec -> fFraction <: rec * fSales       <: rec) recs)
              <+> fNetPrice    =: sum (fmap (\rec -> fFraction <: rec * fSales       <: rec) recs)
                                / sum (fmap (\rec -> fFraction <: rec * fConsumption <: rec) recs)
              <+> fGeometry    =: Geometry
                                    (
                                      "MULTIPOINT("
                                      ++ intercalate ","
                                         [
                                           show (fX <: loc) ++ " " ++ show (fY <: loc)
                                         |
                                           rec <- recs
                                         , let loc' = nodeCube `evaluate` τ rec
                                         , isJust loc'
                                         , let Just loc = loc'
                                         ]
                                      ++ ")"
                                    )
                                  
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

      wktCube :: '[FLocation] *↝ '[FGeometry]
      wktCube =
        fromRecords
          $ [
                  fLocation =: fLocation <: rec
              <+> fGeometry =: Geometry ("POINT(" ++ show (fX <: rec) ++ " " ++ show (fY <: rec) ++ ")")
            |
              rec <- toKnownRecords nodeCube
            ]
            ++
            [
                  fLocation =: fLocation <: rec
              <+> fGeometry =: Geometry ("LINESTRING(" ++ show (fX <: nodeFrom) ++ " " ++ show (fY <: nodeFrom) ++ "," ++ show (fX <: nodeTo) ++ " " ++ show (fY <: nodeTo) ++ ")")
            |
              rec <- toKnownRecords linkCube
            , let nodeFrom = nodeCube ! (fLocation =: fFrom <: rec)
                  nodeTo   = nodeCube ! (fLocation =: fTo   <: rec)
            ]

      wktCube' :: '[FInfrastructure] *↝ '[FGeometry]
      wktCube' = fromRecords $ τ <$> constructions'

      constructions' = [ construction <+> wktCube  ! τ construction | construction <- constructions ]
      flows'         = [ flow         <+> wktCube' ! τ flow         | flow         <- flows         ]
      cashes'        = [ cash         <+> wktCube' ! τ cash         | cash         <- cashes        ]
      impacts'       = [ impact       <+> wktCube' ! τ impact       | impact       <- impacts       ]

    logInfo ""

    logInfo $ "Writing constructions to " ++ show constructionFile ++ " . . ."
    writeFieldCubeFile constructionFile (fromRecords constructions' :: ConstructionCube)
    logInfo $ " . . . " ++ show (length constructions') ++ " records written."

    logInfo $ "Writing flows to " ++ show flowFile ++ " . . ."
    writeFieldCubeFile flowFile (fromRecords flows' :: FlowCube)
    logInfo $ " . . . " ++ show (length flows') ++ " records written."

    logInfo $ "Writing cash to " ++ show cashFile ++ " . . ."
    writeFieldCubeFile cashFile (fromRecords cashes' :: CashCube)
    logInfo $ " . . . " ++ show (length cashes') ++ " records written."

    logInfo $ "Writing impacts to " ++ show impactFile ++ " . . ."
    writeFieldCubeFile impactFile (fromRecords impacts' :: ImpactCube)
    logInfo $ " . . . " ++ show (length impacts') ++ " records written."

    logInfo $ "Writing sales to " ++ show saleFile ++ " . . ."
    writeFieldCubeFile saleFile (saleCube' :: SaleCube)
    logInfo $ " . . . " ++ show (knownSize saleCube') ++ " records written."

    logInfo $ "Writing geometries to " ++ show geometryFile ++ " . . ."
    writeFieldCubeFile geometryFile (geometryCube :: GeometryCube)
    logInfo $ " . . . " ++ show (knownSize geometryCube) ++ " records written."

    logInfo ""
    if failure
      then logCritical "Failure."
      else logInfo "Success."
