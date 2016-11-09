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
import Data.Daft.DataCube (Rekeyer(..), rekey)
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
    regionalIntroductionsSource  :: DataSource Void
  , regionalStockSource          :: DataSource Void
  , stationsSummarySource :: DataSource Void
  , stationsDetailsSource :: DataSource Void
  , sizingParameters   :: StationCapacityParameters 
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ConfigHydrogenSizing

instance ToJSON ConfigHydrogenSizing


-- | Compute introduction years.
calculateHydrogenSizing :: (IsString e, MonadError e m, MonadIO m)
                       => ConfigHydrogenSizing -- ^ Configuration data.
                       -> m ()               -- ^ Action to compute the introduction years.
calculateHydrogenSizing ConfigHydrogenSizing{..} =
  do
    inform $ "Reading regional introductions from " ++ show regionalIntroductionsSource ++ " . . ."
    regionalIntroductions <- readFieldCubeSource regionalIntroductionsSource
    inform $ "Reading vehicle stock from " ++ show regionalStockSource ++ " . . ."
    stock <- readFieldCubeSource regionalStockSource
    let
      (details, summary) = sizeStations sizingParameters regionalIntroductions stock
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


type StationDetailCube = '[FRegion, FYear, FStationID] ↝ '[FNewCapacity]

type StationSummaryCube = '[FYear, FRegion] ↝ '[FSales, FStock, FTravel, FEnergy, FDemand, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]


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


sizeStations :: StationCapacityParameters -> RegionalIntroductionsCube -> StockCube -> (StationDetailCube, StationSummaryCube)
sizeStations parameters introductions stock =
  let
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
        ]
    universe = ω details :: Set (FieldRec '[FStationID])
    summary = stock' ⋈ (κ universe sumCapacities details)
  in
    (
      π (const τ) details
    , summary <> (stock' ⋈ (κ years extendedStock summary))
    )
