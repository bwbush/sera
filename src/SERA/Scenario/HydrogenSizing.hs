-----------------------------------------------------------------------------
--
-- Module      :  SERA.Scenario.HydrogenSizing
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Hydrogen station size computations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Scenario.HydrogenSizing (
-- * Types
  CapitalCostParameters
, SitePreparationParameters
-- * Computation
, capitalCost
, sitePreparationMultiplier
, sizeStations
) where


import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (Rekeyer(..), evaluate, knownEmpty, rekey)
import Data.Daft.Vinyl.FieldCube -- (type (↝), π, σ)
import Data.Daft.Vinyl.FieldCube.IO (showFieldCube)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Default.Util (nan)
import Data.Function (on)
import Data.List (groupBy, intercalate)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Vinyl.Lens (type (∈))
import GHC.Generics (Generic)
import SERA.Refueling.Hydrogen.Sizing (StationCapacityParameters, stationCapacitiesByQuantile)
import SERA.Refueling.Types
import SERA.Scenario.Types (RegionalIntroductionsCube, FFirstYear, fFirstYear, fIntroductionYear, FLastYear, fLastYear, FStationCount, fStationCount, hasStations)
import SERA.Service ()
import SERA.Types.Fields hiding (FSales, fSales) -- (Region(..), FRegion, fRegion, UrbanCode(..), FUrbanCode, fUrbanCode, UrbanName(..), FUrbanName, fUrbanName, FYear, fYear)
import SERA.Vehicle.Stock.Types (StockCube)
import SERA.Vehicle.Types


-- | Capital cost parameters.
data CapitalCostParameters =
  CapitalCostParameters
  {
    costReference     :: Double -- ^ Reference cost.
  , capacityReference :: Double -- ^ Reference capacity.
  , capacityExponent  :: Double -- ^ Exponent for scaling with capacity.
  , quantityReference :: Double -- ^ Reference industry-wide cumulative capacity.
  , quantityExponent  :: Double -- ^ Exponent for scaling with industry-wide cumulative capacity.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON CapitalCostParameters

instance ToJSON CapitalCostParameters


-- | Capital cost for a new station.
capitalCost :: CapitalCostParameters -- ^ Capital cost parameters.
            -> Double                -- ^ Industry-wide cumulative capacity.
            -> Double                -- ^ Capacity for new station.
            -> Double                -- ^ Cost of new station.
capitalCost CapitalCostParameters{..} quantity capacity =
  costReference * (capacity / capacityReference)**capacityExponent * (quantity / quantityReference)**quantityExponent


-- | Site preparation parameters.
data SitePreparationParameters =
  SitePreparationParameters
  {
    preparationYear      :: Double -- ^ The reference year for the multiplier.
  , preparationMultipler :: Double -- ^ The multiplier at the reference year.
  , preparationReduction :: Double -- ^ The amount the multiplier is reduced each year.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON SitePreparationParameters

instance ToJSON SitePreparationParameters


-- | Compute the extra site preparation cost.
sitePreparationMultiplier :: SitePreparationParameters -- ^ Site preparation parameters.
                          -> Year                      -- ^ The year.
                          -> Double                    -- ^ The site preparation cost multiplier for the year.
sitePreparationMultiplier SitePreparationParameters{..} year =
  maximum
    [
      1
    , preparationMultipler - preparationReduction * (fromIntegral year - preparationYear)
    ]


-- | Field type for a list of years and station capacities.
type FStationList = '("Station List", [(Year, StationID, Double)])


-- | Field label for a list of years and station capacities.
fStationList :: SField FStationList
fStationList = SField


-- | Determine whether there are new stations for the particular key.
hasNewStation :: (FNewElectrolysisCapacity ∈ vs, FNewPipelineCapacity ∈ vs, FNewOnSiteSMRCapacity ∈ vs, FNewGH2TruckCapacity ∈ vs, FNewLH2TruckCapacity ∈ vs)
              => k           -- ^ The key.
              -> FieldRec vs -- ^ The capacity information.
              -> Bool        -- ^ Whether there are new stations.
hasNewStation _ rec =
     0 < fNewElectrolysisCapacity <: rec
  || 0 < fNewPipelineCapacity <: rec
  || 0 < fNewOnSiteSMRCapacity <: rec
  || 0 < fNewGH2TruckCapacity <: rec
  || 0 < fNewLH2TruckCapacity <: rec


-- | Find the last year 
lastOverride :: FieldRec '[FRegion]                -- ^ The region.
             -> [FieldRec '[FYear, FStationCount]] -- ^ The the overridden stations.
             -> FieldRec '[FFirstYear, FLastYear, FStationCount]   -- ^ The last year.
lastOverride _ rec =
      fFirstYear    =: minimum (fromIntegral . (fYear <:) <$> rec)
  <+> fLastYear     =: maximum (fromIntegral . (fYear <:) <$> rec)
  <+> fStationCount =: sum ((fStationCount <:) <$> rec)


-- | Totatl the vehicle stock.
totalStock :: FieldRec '[FYear, FRegion]                            -- ^ The key.
           -> [FieldRec '[FSales, FStock, FTravel, FEnergy]]        -- ^ The stocks.
           -> FieldRec '[FSales, FStock, FTravel, FEnergy, FDemand] -- ^ The totals.
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


-- | Size stations.
sizing :: StationCapacityParameters                   -- ^ Station capacity parameters.
       -> FieldRec '[FRegion]                         -- ^ The region.
       -> [FieldRec '[FStationCount, FYear, FDemand]] -- ^ The number of stations and demand for the year.
       -> FieldRec '[FStationList]                    -- ^ The station sizes.
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


-- | Drop the count of stations from the value.
dropStationCount :: k                                         -- ^ The key.
                 -> FieldRec '[FStationCount, FYear, FDemand] -- ^ The value.
                 -> FieldRec '[FYear, FDemand]                -- ^ The value without station count.
dropStationCount _ = τ


-- | Sum the station capacities.
sumCapacities :: FieldRec '[FRegion, FYear]                                                                                                                                                                                                                                                                                                                                                            -- ^ The key.
              -> [FieldRec '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction]] -- ^ The capacities.
              -> FieldRec '[FYear, FNewStations, FNewCapacity]
                                                                                                                                                                                                                                                             -- ^ The totals.
sumCapacities key recs =
  let
    newCapacity :: FieldRec '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction] -> Double
    newCapacity rec =
        fNewElectrolysisCapacity <: rec
      + fNewPipelineCapacity <: rec
      + fNewOnSiteSMRCapacity <: rec
      + fNewGH2TruckCapacity <: rec
      + fNewLH2TruckCapacity <: rec
    newCapacities = filter (> 0) $ map newCapacity recs
  in
        fYear        =: fYear <: key
    <+> fNewStations =: length newCapacities
    <+> fNewCapacity =: sum    newCapacities


-- | Compute running totals.
runningTotals :: '[FRegion, FYear, FStationID] ↝ '[FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction] -- ^ The data cube.
              -> '[FRegion, FYear] ↝ '[FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]                                                                                                                                                                                -- ^ The running totals for the cube.
runningTotals cube =
  let
    stations = ω cube :: Set (FieldRec '[FStationID])
    runningTotal :: [(Int, (Int, Double))] -> [(Int, ((Int, Int), (Double, Double)))]
    runningTotal [] = []
    runningTotal ((y, (s, t)) : z) = runningTotal' (y, ((s, s), (t, t))) z
    runningTotal' :: (Int, ((Int, Int), (Double, Double))) -> [(Int, (Int, Double))] -> [(Int, ((Int, Int), (Double, Double)))]
    runningTotal' (y, ((s, st), (c, ct))) []                   =
      (y, ((s, st), (c, ct))) : if y >= 2050 -- FIXME
                                  then []
                                  else runningTotal' (y + 1, ((0,       st), (0,       ct))) []
    runningTotal' (y, ((s, st), (c, ct))) (z@(y', (s', c')) : zs) =
      (y, ((s, st), (c, ct))) : if y + 1 == y'
                                  then runningTotal' (y + 1, ((s', s' + st), (c', c' + ct))) zs
                                  else runningTotal' (y + 1, ((0 ,      st), (0 ,      ct))) (z : zs)
  in
    ε $ fromRecords
      [
            fRegion =: region
        <+> fYear   =: year
        <+> fNewStations =: newStations
        <+> fTotalStations =: totalStations
        <+> fNewCapacity =: newCapacity
        <+> fTotalCapacity =: totalCapacity
      |
        recs <- groupBy ((==) `on` (fRegion <:)) $ toKnownRecords $ κ stations sumCapacities cube
      , let region = fRegion <: head recs
      , (year, ((newStations, totalStations), (newCapacity, totalCapacity)))
          <- runningTotal
               [
                 (fYear <: rec, (fNewStations <: rec, fNewCapacity <: rec))
               |
                 rec <- recs
               ]
      ]


-- | Extend the stock data to later years.
extendedStock :: FieldRec '[FRegion]                                                                                                 -- ^ The key.
              -> [FieldRec '[FSales, FStock, FTravel, FEnergy, FDemand, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]] -- ^ The stock information.
              -> FieldRec '[FNewStations, FTotalStations, FNewCapacity, FTotalCapacity]                                              -- ^ The extension of the value.
extendedStock _ recs =
      fNewStations =: 0
  <+> fTotalStations =: maximum (map (fTotalStations <:) recs)
  <+> fNewCapacity =: 0
  <+> fTotalCapacity =: maximum (map (fTotalCapacity <:) recs)


-- | Size refueling stations.
sizeStations :: StationCapacityParameters               -- ^ The station capacity parameters.
             -> CapitalCostParameters                   -- ^ The station cost parameters.
             -> SitePreparationParameters               -- ^ The site preperation parameters.
             -> GlobalCapacityCube                      -- ^ The station capacity outside the region.
             -> StationDetailCube                       -- ^ The station details for manually input stations.
             -> RegionalIntroductionsCube               -- ^ The regional introduction years.
             -> StockCube                               -- ^ The regional vehicle stock.
             -> (StationDetailCube, StationSummaryCube, Maybe String) -- ^ The station details and summary.
sizeStations parameters parameters' parameters'' externals overrides introductions stock =
  let
    regionStations = ω overrides :: Set (FieldRec '[FYear, FStationID])
    pullYear key _ =
          fYear         =: fYear <: key
      <+> fStationCount =: 1
    overrides' =
      κ regionStations lastOverride
      $ π pullYear
      $ σ hasNewStation overrides
    urbanToRegion = -- FIXME: A very similar function exists in SERA.Scenario.Regionalization.
      rekey
        $ Rekeyer
            (\key -> fRegion =: Region (region (fRegion <: key) ++ " | " ++ urbanCode (fUrbanCode <: key) ++ " | " ++ urbanName (fUrbanName <: key)))
            undefined
    introductions' = urbanToRegion $ σ hasStations introductions
    vocationsVehicles = ω stock :: Set (FieldRec '[FVocation, FVehicle])
    stock' = κ vocationsVehicles totalStock stock
    years = ω stock :: Set (FieldRec '[FYear])
    dailyDemands key rec =
      let
        stationCount = fStationCount        <: rec
        year         = fYear                <: key
        totalDemand  = fDemand              <: rec
      in
            fStationCount =: stationCount
        <+> fYear         =: year
        <+> fDemand       =: totalDemand
    daily =
      π dailyDemands
        $ stock' ⋈ introductions'
    capacities =
      toKnownRecords
        $ κ years (sizing parameters)
        $ (
            (δ years (const τ) overrides' :: '[FYear, FRegion] ↝  '[FStationCount])
          ⋈ π dropStationCount daily :: '[FYear, FRegion] ↝  '[FStationCount, FYear, FDemand]
          )
        <> daily
    details =
      ε $ fromRecords
        [
              fRegion                  =: fRegion <: rec
          <+> fYear                    =: y
          <+> fStationID               =: s
          <+> fNewCapitalCost          =: nan
          <+> fNewInstallationCost     =: 0
          <+> fNewCapitalIncentives    =: 0
          <+> fNewProductionIncentives =: 0
          <+> fNewElectrolysisCapacity =: (if c <= 100            then c    else 0)
          <+> fNewPipelineCapacity     =: 0
          <+> fNewOnSiteSMRCapacity    =: 0
          <+> fNewGH2TruckCapacity     =: (if c > 100 && c <= 200 then c    else 0)
          <+> fNewLH2TruckCapacity     =: (if            c >  200 then c    else 0)
          <+> fRenewableFraction       =: (if california          then 0.33 else 0)
        |
          rec <- capacities
        , let ysc = fStationList <: rec
        , (y, s, c) <- ysc
        , maybe True ((fromIntegral y >) . (fLastYear <:)) $ overrides' `evaluate` τ rec
        , let california = take 2 (show $ fRegion <: rec) == "CA"
        ]
    details' = overrides <> details
    padding = κ (undefined :: Set (FieldRec '[FStationID])) (\_ _ -> fSales =: 0 <+> fStock =: 0 <+> fTravel =: 0 <+> fEnergy =: 0 <+> fDemand =: 0) details'
    summary = (stock' <> padding) ⋈ runningTotals details'
    regions = ω summary :: Set (FieldRec '[FRegion])
    totalGlobalCapacity key recs =
      fTotalCapacity =: maybe 0 (fTotalCapacity <:) (externals `evaluate` τ key) + sum ((fTotalCapacity <:) <$> recs)
    inconsistent =
      σ (\_ rec -> fromIntegral (fIntroductionYear <: rec) < fFirstYear <: rec)
        $ π (\_ rec -> fIntroductionYear =: fIntroductionYear <: rec) introductions'
       ⋈  π (\_ rec -> fFirstYear =: fFirstYear <: rec) overrides'
    global = κ regions totalGlobalCapacity summary <> externals
    price key rec =
      let
        year     = fYear <: key
        quantity = maybe 0 (fTotalCapacity <:) $ global `evaluate` τ key
        capacity = fNewElectrolysisCapacity <: rec
                   + fNewPipelineCapacity <: rec
                   + fNewOnSiteSMRCapacity <: rec
                   + fNewGH2TruckCapacity <: rec
                   + fNewLH2TruckCapacity <: rec
        cost = capitalCost parameters' quantity capacity * sitePreparationMultiplier parameters'' year
      in
        if isNaN (fNewCapitalCost <: rec)
          then fNewCapitalCost =: cost <+> τ rec
          else rec
  in
    (
      π price details'
    , summary <> ((stock' <> padding) ⋈ κ years extendedStock summary)
    , if knownEmpty inconsistent
        then Nothing
        else
          Just
            $ unlines
            $ ("" :)
            $ ("**** Inconsistencies found between manually specified introduction years and manually specified planned stations:" :)
            $ ("" :)
            $ map (intercalate "\t")
            $ showFieldCube inconsistent
    )
