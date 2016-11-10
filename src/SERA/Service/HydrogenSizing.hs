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
-- | Services for estimating introduction years.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Service.HydrogenSizing (
-- * Configuration
  ConfigHydrogenSizing(..)
-- * Computation
, calculateHydrogenSizing
) where


import Control.Arrow (first)
import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withText)
import Data.Daft.DataCube (Rekeyer(..), evaluate, rekey)
import Data.Daft.Source (DataSource(..), withSource)
import Data.Daft.Vinyl.FieldCube -- (type (↝), π, σ)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeSource, writeFieldCubeSource)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Default (Default(..))
import Data.List (inits)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.String (IsString)
import Data.String.ToString (toString)
import Data.Tuple.Util (trd3)
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Void (Void)
import GHC.Generics (Generic)
import SERA (inform)
import SERA.Refueling.Hydrogen.Sizing (StationCapacityParameters, stationCapacitiesByQuantile)
import SERA.Scenario.Introduction (FCoverageStations, FIntroductionYear, RegionalIntroductionsCube, FMaximumStations, fMaximumStations, FStationCount, fStationCount, FThreshholdStations)
import SERA.Service ()
import SERA.Types -- (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName, FYear, fYear)
import SERA.Vehicle.Stock.Types (StockCube)
import SERA.Vehicle.Types


newtype StationID = StationID {stationID :: String}
  deriving (Default, Eq, Generic, Ord)

instance Read StationID where
  readsPrec
    | quotedStringTypes = (fmap (first StationID) .) . readsPrec
    | otherwise         = const $ return . (, []) . StationID

instance Show StationID where
  show
    | quotedStringTypes = show . stationID
    | otherwise         = stationID

instance FromJSON StationID where
  parseJSON = withText "SERA.Types.StationID" $ return . StationID . toString

instance ToJSON StationID where
  toJSON = toJSON . stationID


type FStationID = '("Station ID", StationID)


fStationID :: SField FStationID
fStationID = SField


-- | Configuration for vehicle stock modeling.
data ConfigHydrogenSizing =
  ConfigHydrogenSizing
  {
    externalCapacitySource        :: DataSource Void
  , regionalIntroductionsSource  :: DataSource Void
  , regionalStockSource          :: DataSource Void
  , overrideStationsSource       :: DataSource Void
  , stationsSummarySource :: DataSource Void
  , stationsDetailsSource :: DataSource Void
  , sizingParameters   :: StationCapacityParameters 
  , capitalCostParameters :: CapitalCostParameters
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigHydrogenSizing

instance ToJSON ConfigHydrogenSizing


data CapitalCostParameters =
  CapitalCostParameters
  {
    costReference    :: Double
  , capacityReference :: Double
  , capacityExponent :: Double
  , quantityReference    :: Double
  , quantityExponent :: Double
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON CapitalCostParameters

instance ToJSON CapitalCostParameters


capitalCost :: CapitalCostParameters -> Double -> Double -> Double
capitalCost CapitalCostParameters{..} quantity capacity =
  costReference * (capacity / capacityReference)**capacityExponent * (quantity / quantityReference)**quantityExponent


-- | Compute introduction years.
calculateHydrogenSizing :: (IsString e, MonadError e m, MonadIO m)
                       => ConfigHydrogenSizing -- ^ Configuration data.
                       -> m ()               -- ^ Action to compute the introduction years.
calculateHydrogenSizing ConfigHydrogenSizing{..} =
  do
    inform $ "Reading external capacity from " ++ show externalCapacitySource ++ " . . ."
    externalCapacity <- readFieldCubeSource externalCapacitySource
    inform $ "Reading regional introductions from " ++ show regionalIntroductionsSource ++ " . . ."
    regionalIntroductions <- readFieldCubeSource regionalIntroductionsSource
    inform $ "Reading vehicle stock from " ++ show regionalStockSource ++ " . . ."
    stock <- readFieldCubeSource regionalStockSource
    inform $ "Reading overridden stations from " ++ show overrideStationsSource ++ " . . ."
    overrides <- readFieldCubeSource overrideStationsSource
    let
      (details, summary) = sizeStations sizingParameters capitalCostParameters externalCapacity overrides regionalIntroductions stock
    withSource stationsSummarySource $ \source -> do
      inform $ "Writing station summary to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source summary
    withSource stationsDetailsSource $ \source -> do
      inform $ "Writing station details to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source details



type Capacity = Double

type FNewCapacity = '("New Capacity [kg/day]", Capacity)

fNewCapacity :: SField FNewCapacity
fNewCapacity = SField


type FTotalCapacity = '("Total Capacity [kg/day]", Capacity)

fTotalCapacity :: SField FTotalCapacity
fTotalCapacity = SField


type FNewStations = '("New Stations", Int)

fNewStations :: SField FNewStations
fNewStations = SField


type FTotalStations = '("Total Stations", Int)

fTotalStations :: SField FTotalStations
fTotalStations = SField


type FNewCapitalCost = '("New Capital Cost [$]", Double)

fNewCapitalCost :: SField FNewCapitalCost
fNewCapitalCost = SField


type FNewInstallationCost = '("New Installation Cost [$]", Double)

fNewInstallationCost :: SField FNewInstallationCost
fNewInstallationCost = SField


type FNewCapitalIncentives = '("New Capital Incentives [$]", Double)

fNewCapitalIncentives :: SField FNewCapitalIncentives
fNewCapitalIncentives = SField


type FNewProductionIncentives = '("New Production Incentives [$]", Double)

fNewProductionIncentives :: SField FNewProductionIncentives
fNewProductionIncentives = SField


type FNewElectrolysisCapacity = '("New Electrolysis [kg/day]", Capacity)

fNewElectrolysisCapacity :: SField FNewElectrolysisCapacity
fNewElectrolysisCapacity = SField


type FNewPipelineCapacity = '("New Pipeline [kg/day]", Capacity)

fNewPipelineCapacity :: SField FNewPipelineCapacity
fNewPipelineCapacity = SField


type FNewOnSiteSMRCapacity = '("New On-Site SMR [kg/day]", Capacity)

fNewOnSiteSMRCapacity :: SField FNewOnSiteSMRCapacity
fNewOnSiteSMRCapacity = SField


type FNewGH2TruckCapacity = '("New GH2 [kg/day]", Capacity)

fNewGH2TruckCapacity :: SField FNewGH2TruckCapacity
fNewGH2TruckCapacity = SField


type FNewLH2TruckCapacity = '("New LH2 [kg/day]", Capacity)

fNewLH2TruckCapacity :: SField FNewLH2TruckCapacity
fNewLH2TruckCapacity = SField


type FRenewableFraction = '("Renewable Fraction [kg/kg]", Double)

fRenewableFraction :: SField FRenewableFraction
fRenewableFraction = SField


type StationDetailCube = '[FRegion, FYear, FStationID] ↝ '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction]


type StationSummaryCube = '[FYear, FRegion] ↝ '[FSales, FStock, FTravel, FEnergy, FDemand, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]


totalGlobalCapacity :: GlobalCapacityCube -> FieldRec '[FYear] -> [FieldRec '[FSales, FStock, FTravel, FEnergy, FDemand, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]] -> FieldRec '[FTotalCapacity]
totalGlobalCapacity global key recs =
  fTotalCapacity =: maybe 0 (fTotalCapacity <:) (global `evaluate` τ key) + sum ((fTotalCapacity <:) <$> recs)


type GlobalCapacityCube = '[FYear] ↝ '[FTotalCapacity]


type Demand = Double

type FDemand = '("Demand [kg/day]", Demand)

fDemand :: SField FDemand
fDemand = SField


type FStationList = '("Station List", [(Year, StationID, Capacity)])

fStationList :: SField FStationList
fStationList = SField


dailyDemands :: FieldRec '[FYear, FRegion] -> FieldRec '[FSales, FStock, FTravel, FEnergy, FDemand, FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations] ->  FieldRec '[FStationCount, FYear, FDemand]
dailyDemands key rec =
  let
    stationCount = fStationCount        <: rec
    year         = fYear                <: key
    totalDemand  = fDemand              <: rec
  in
        fStationCount =: stationCount
    <+> fYear         =: year
    <+> fDemand       =: totalDemand


sizing :: StationCapacityParameters -> FieldRec '[FRegion] -> [FieldRec '[FStationCount, FYear, FDemand]] ->  FieldRec '[FStationList]
sizing parameters key recs =
  let
    region = fRegion <: key
    initialStations = fromIntegral $ fStationCount <: head recs
    years = reverse $ map (fYear <:) recs
    demands = reverse $ map (fDemand <:) recs
    capacities = stationCapacitiesByQuantile parameters initialStations demands
    label =
      init
      . takeWhile (/= '|')
      . drop 2
      . dropWhile (/= '|')
      . show
  in
    fStationList =: concat (zipWith (\y cs -> zipWith (y, , ) (map (\s -> StationID $ "Generic Station " ++ label region ++ ":" ++ show y ++ ":" ++ show s) [(1::Int)..]) cs) years capacities)


sumCapacities :: FieldRec '[FRegion, FYear] -> [FieldRec '[FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]] -> FieldRec '[FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]
sumCapacities _ recs = 
      fNewStations =: sum (map (fNewStations <:) recs)
  <+> fTotalStations =: maximum (map (fTotalStations <:) recs)
  <+> fNewCapacity =: sum (map (fNewCapacity <:) recs)
  <+> fTotalCapacity =: maximum (map (fTotalCapacity <:) recs)


totalStock :: FieldRec '[FYear, FRegion] -> [FieldRec '[FSales, FStock, FTravel, FEnergy]] -> FieldRec '[FSales, FStock, FTravel, FEnergy, FDemand]
totalStock _ rec =
  let
    sales = sum $ (fSales <:) <$> rec
    stock = sum $ (fStock <:) <$> rec
    travel = sum $ (fTravel <:) <$> rec
    energy = sum $ (fEnergy <:) <$> rec
    demand = 120.1e6 / 121.3e6 / 365 * energy
  in
        fSales =: sales
    <+> fStock =: stock
    <+> fTravel =: travel
    <+> fEnergy =: energy
    <+> fDemand =: demand


extendedStock :: FieldRec '[FRegion] -> [FieldRec '[FSales, FStock, FTravel, FEnergy, FDemand, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]] -> FieldRec '[FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]
extendedStock _ recs =
      fNewStations =: 0
  <+> fTotalStations =: maximum (map (fTotalStations <:) recs)
  <+> fNewCapacity =: 0
  <+> fTotalCapacity =: maximum (map (fTotalCapacity <:) recs)


urbanToRegion :: '[FRegion, FUrbanCode, FUrbanName] ↝ v -> '[FRegion] ↝ v
urbanToRegion =
  rekey
    $ Rekeyer
        (\key -> fRegion =: Region (region (fRegion <: key) ++ " | " ++ urbanCode (fUrbanCode <: key) ++ " | " ++ urbanName (fUrbanName <: key)))
        undefined


hasStations :: k -> FieldRec '[FRelativeMarketShare, FIntroductionYear, FStationCount, FCoverageStations, FThreshholdStations, FMaximumStations] -> Bool
hasStations = const $ (/= 0) . (fMaximumStations <:)


prepare :: FieldRec '[FRegion, FYear, FStationID] -> FieldRec '[FNewStations, FTotalStations, FNewCapacity, FTotalCapacity] -> FieldRec '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction]
prepare key rec =
  let
    california = take 2 (show $ fRegion <: key) == "CA"
    capacity = fNewCapacity <: rec
  in
        fNewCapitalCost          =: read "NaN" -- FIXME
    <+> fNewInstallationCost     =: 0
    <+> fNewCapitalIncentives    =: 0
    <+> fNewProductionIncentives =: 0
    <+> fNewElectrolysisCapacity =: (if capacity <= 100                   then capacity else 0)
    <+> fNewPipelineCapacity     =: 0
    <+> fNewOnSiteSMRCapacity    =: 0
    <+> fNewGH2TruckCapacity     =: (if capacity > 100 && capacity <= 200 then capacity else 0)
    <+> fNewLH2TruckCapacity     =: (if                   capacity >  200 then capacity else 0)
    <+> fRenewableFraction       =: (if california                        then 0.33     else 0)


hasNewStation :: k -> FieldRec '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction] -> Bool
hasNewStation _ rec =
     0 < fNewElectrolysisCapacity <: rec
  || 0 < fNewPipelineCapacity <: rec
  || 0 < fNewOnSiteSMRCapacity <: rec
  || 0 < fNewGH2TruckCapacity <: rec
  || 0 < fNewLH2TruckCapacity <: rec


pullYear :: FieldRec '[FRegion, FYear, FStationID] -> v -> FieldRec '[FYear]
pullYear key _ = τ key


lastOverride :: FieldRec '[FRegion] -> [FieldRec '[FYear]] -> FieldRec '[FYear]
lastOverride _ rec = fYear =: maximum ((fYear <:) <$> rec)


sizeStations :: StationCapacityParameters -> CapitalCostParameters -> GlobalCapacityCube -> StationDetailCube -> RegionalIntroductionsCube -> StockCube -> (StationDetailCube, StationSummaryCube)
sizeStations parameters parameters' externals overrides introductions stock =
  let
    regionStations = ω overrides :: Set (FieldRec '[FYear, FStationID])
    overrides' :: '[FRegion] ↝ '[FYear]
    overrides' =
      κ regionStations lastOverride
      $ π pullYear
      $ σ hasNewStation overrides
    introductions' = urbanToRegion $ σ hasStations introductions
    vocationsVehicles = ω stock :: Set (FieldRec '[FVocation, FVehicle])
    stock' = κ vocationsVehicles totalStock stock
    years = ω stock :: Set (FieldRec '[FYear])
    capacities =
      toKnownRecords
        $ κ years (sizing parameters)
        $ π dailyDemands
        $ stock' ⋈ introductions'
      :: [FieldRec '[FRegion, FStationList]]
    details =
      fromRecords
        [
              fRegion        =: fRegion <: rec
          <+> fYear          =: y
          <+> fStationID     =: s
          <+> fNewStations   =: 1
          <+> fTotalStations =: n
          <+> fNewCapacity   =: c
          <+> fTotalCapacity =: t
          :: FieldRec '[FRegion, FYear, FStationID, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]
        |
          rec <- capacities
        , let ysc = fStationList <: rec
        , let runningCounts = map sum . tail . inits . map (const 1) $ ysc
        , let runningCapacities = map sum . tail . inits . map trd3 $ ysc
        , ((y, s, c), n, t) <- zip3 ysc runningCounts runningCapacities
        , maybe True ((y >=) . (fYear <:)) $ overrides' `evaluate` τ rec
        ]
        :: '[FRegion, FYear, FStationID] ↝ '[FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]
    universe = ω details :: Set (FieldRec '[FStationID])
    summary = stock' ⋈ (κ universe sumCapacities details)
    regions = ω summary :: Set (FieldRec '[FRegion])
    global = κ regions (totalGlobalCapacity externals) summary <> externals
    price :: FieldRec '[FRegion, FYear, FStationID] -> FieldRec '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction] -> FieldRec '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction]
    price key rec =
      let
        quantity = maybe 0 (fTotalCapacity <:) $ global `evaluate` τ key
        capacity = fNewElectrolysisCapacity <: rec
                   + fNewPipelineCapacity <: rec
                   + fNewOnSiteSMRCapacity <: rec
                   + fNewGH2TruckCapacity <: rec
                   + fNewLH2TruckCapacity <: rec
        cost = capitalCost parameters' quantity capacity
      in
        if isNaN (fNewCapitalCost <: rec)
          then fNewCapitalCost =: cost <+> τ rec
          else rec
  in
    (
      π price $ overrides <> π prepare details
    , summary <> (stock' ⋈ (κ years extendedStock summary))
    )
