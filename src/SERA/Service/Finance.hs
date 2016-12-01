{-# LANGUAGE DataKinds                 #-}
-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service.Finance
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Station finance computations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeOperators             #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}


module SERA.Service.Finance (
-- * Types
  Inputs(..)
, StationParameters(..)
, DoubleSeries
-- * Computation
, financeMain
) where


-- FIXME: This module needs major cleanup.


import Control.Arrow ((&&&), first)
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), withText, defaultOptions, genericToJSON)
import Data.Daft.DataCube (evaluate)
import Data.Daft.Source (DataSource(..))
import Data.Daft.Vinyl.FieldCube -- (type (↝), π, σ)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeSource)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Default.Util (zero, nan)
import Data.Function (on)
import Data.List (groupBy, intercalate, sortBy, transpose, zipWith4)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isNothing)
import Data.String (IsString)
import Data.String.ToString (toString)
import Data.Table (Tabulatable(..))
import Data.Vinyl.Derived (FieldRec, SField(..))
import Data.Void (Void)
import GHC.Generics (Generic)
import Math.GeometricSeries (GeometricSeries(..), asFunction)
import SERA (unsafeInform)
import SERA.Configuration.ScenarioInputs (ScenarioInputs(..))
import SERA.Finance.Analysis (computePerformanceAnalyses)
import SERA.Finance.Analysis.CashFlowStatement (CashFlowStatement(..))
import SERA.Finance.Analysis.Finances (Finances(..))
import SERA.Finance.Analysis.PerformanceAnalysis (PerformanceAnalysis)
import SERA.Finance.Capital (Capital(..))
import SERA.Finance.IO.Xlsx (formatResultsAsFile)
import SERA.Finance.Scenario (Scenario(..))
import SERA.Finance.Solution (solveConstrained')
import SERA.Refueling.Types
import SERA.Service ()
import SERA.Types
import SERA.Util.Summarization (summation)
import SERA.Vehicle.Types


-- | Parameters for finance computations.
data Inputs =
  Inputs
    {
      referenceYear         :: Int
    , cohortStart           :: Int
    , cohortFinish          :: Int
    , scenario              :: ScenarioInputs
    , station               :: StationParameters
    , feedstockUsageSource  :: DataSource Void
    , energyPricesSource    :: DataSource Void
    , carbonCreditSource    :: DataSource Void
    , stationsSummarySource :: DataSource Void
    , stationsDetailsSource :: DataSource Void
    , financesDirectory     :: FilePath
    , financesSpreadsheet   :: FilePath
    , financesFile          :: FilePath
    , targetMargin          :: Maybe Double
    }
    deriving (Generic, Read, Show)

instance FromJSON Inputs

instance ToJSON Inputs where
  toJSON = genericToJSON defaultOptions


-- | A geometric series of floating point values.
type DoubleSeries = GeometricSeries Double


-- | Financial parameters for stations.
data StationParameters =
  StationParameters
  {
    maintenanceCostFraction         :: Double
  , staffing                        :: Double
  , sellingAndAdministrativeExpense :: Double
  , creditCardFeesRate              :: Double
  , salesTaxRate                    :: Double
  , propertyTaxRate                 :: Double
  , propertyInsuranceRate           :: Double
  , incidentalRevenueFunction       :: DoubleSeries
  , licensingAndPermittingFunction  :: DoubleSeries
  , rentOfLandFunction              :: DoubleSeries
  , laborRateFunction               :: DoubleSeries
  , roadTaxFunction                 :: DoubleSeries
  } deriving (Generic, Read, Show)

instance FromJSON StationParameters

instance ToJSON StationParameters


newtype HydrogenSource = HydrogenSource {hydrogenSource :: String}
  deriving (Eq, Ord)

instance Read HydrogenSource where
  readsPrec
    | quotedStringTypes = (fmap (first HydrogenSource) .) . readsPrec
    | otherwise         = const $ return . (, []) . HydrogenSource

instance Show HydrogenSource where
  show
    | quotedStringTypes = show . hydrogenSource
    | otherwise         = hydrogenSource

instance FromJSON HydrogenSource where
  parseJSON = withText "HydrogenSource" $ return . HydrogenSource . toString

instance ToJSON HydrogenSource where
  toJSON = toJSON . hydrogenSource

type FHydrogenSource = '("Hydrogen Source", HydrogenSource)

fHydrogenSource :: SField FHydrogenSource
fHydrogenSource = SField

newtype FeedstockType = FeedstockType {feedstockType :: String}
  deriving (Eq, Ord)

instance Read FeedstockType where
  readsPrec
    | quotedStringTypes = (fmap (first FeedstockType) .) . readsPrec
    | otherwise         = const $ return . (, []) . FeedstockType

instance Show FeedstockType where
  show
    | quotedStringTypes = show . feedstockType
    | otherwise         = feedstockType

instance FromJSON FeedstockType where
  parseJSON = withText "FeedstockType" $ return . FeedstockType . toString

instance ToJSON FeedstockType where
  toJSON = toJSON . feedstockType

type FFeedstockType = '("Feedstock", FeedstockType)

fFeedstockType :: SField FFeedstockType
fFeedstockType = SField

type FFeedstockUsage = '("Feedstock Usage [/kg]", Double)

fFeedstockUsage :: SField FFeedstockUsage
fFeedstockUsage = SField

type FeedstockUsageCube = '[FHydrogenSource, FFeedstockType] ↝ '[FFeedstockUsage]


type FStationUtilization = '("Utilization [kg/kg]", Double)

fStationUtilization :: SField FStationUtilization
fStationUtilization = SField


type FNonRenewablePrice = '("Non-Renewable Price [$]", Double)

fNonRenewablePrice :: SField FNonRenewablePrice
fNonRenewablePrice = SField

type FRenewablePrice = '("Renewable Price [$]", Double)

fRenewablePrice :: SField FRenewablePrice
fRenewablePrice = SField


type EnergyPriceCube = '[FYear, FFeedstockType] ↝ '[FNonRenewablePrice, FRenewablePrice]


type StationUtilizationCube = '[FYear, FRegion] ↝ '[FStationUtilization]


type FNonRenewableCredit = '("Carbon Credit (Non-Renewable) [$/kg]", Double)

fNonRenewableCredit :: SField FNonRenewableCredit
fNonRenewableCredit = SField


type FRenewableCredit = '("Carbon Credit (Renewable) [$/kg]", Double)

fRenewableCredit :: SField FRenewableCredit
fRenewableCredit = SField

type CarbonCreditCube = '[FHydrogenSource] ↝ '[FNonRenewableCredit, FRenewableCredit]


computeRegionalUtilization :: StationSummaryCube -> StationUtilizationCube
computeRegionalUtilization =
  let
    utilization :: k -> FieldRec '[FSales, FStock, FTravel, FEnergy, FDemand, FNewStations, FTotalStations, FNewCapacity, FTotalCapacity] -> FieldRec '[FStationUtilization]
    utilization _ rec =
      fStationUtilization =: fDemand <: rec / fTotalCapacity <: rec
  in
    π utilization


financeMain :: (IsString e, MonadError e m, MonadIO m)
                       => Inputs -- ^ Configuration data.
                       -> m ()               -- ^ Action to compute the introduction years.
financeMain parameters@Inputs{..}=
  do
    feedstockUsage <- readFieldCubeSource feedstockUsageSource
    energyPrices <- readFieldCubeSource energyPricesSource
    carbonCredits <- readFieldCubeSource carbonCreditSource
    stationsSummary <- readFieldCubeSource stationsSummarySource
    stationsDetail <- readFieldCubeSource stationsDetailsSource
    let
      regionalUtilization = computeRegionalUtilization stationsSummary
      prepared = makeInputs parameters feedstockUsage energyPrices carbonCredits regionalUtilization stationsDetail
      ids = map ((\(reg, _, _) -> reg) . head) prepared
      prepared' = case targetMargin of
        Nothing     -> prepared
        Just margin -> let
                         refresh prepared'' =
                           let
                             (_, allOutputs') = multiple parameters prepared''
                             prices = findHydrogenPrice margin allOutputs'
                           in
                             map (setHydrogenPrice prices) prepared''
                       in
                         (refresh . refresh . refresh . refresh . refresh . refresh . refresh . refresh . refresh . refresh) prepared
      (outputs, allOutputs) = multiple parameters prepared'
    liftIO $ sequence_
      [
        formatResultsAsFile outputFile' $ dumpOutputs' output
      |
        (idx, output) <- zip ids outputs
      , let outputFile' = financesDirectory ++ "/" ++ map (\x -> case x of ':' -> '_' ; '/' -> '_'; '\\' -> '_' ; y -> y) idx ++ ".xlsx"
      ]
    liftIO $ formatResultsAsFile financesSpreadsheet $ dumpOutputs' allOutputs
    liftIO $ writeFile financesFile $ dumpOutputs9 allOutputs


multiple :: Inputs -> [[(String, Capital, Scenario)]] -> ([Outputs], Outputs)
multiple parameters capitalScenarios' =
  let
    capitalScenarios = map (map (\(_, s, t) -> (s, t))) capitalScenarios'
    firstYear = minimum $ map (stationYear . fst . head) capitalScenarios
    outputs = map (single parameters) capitalScenarios
    outputs' = outputs
    scenarioDefinition'= scenario parameters
    stations'  = map summation $ transpose $ map (padStations firstYear  . stations ) outputs'
    scenarios' = map summation $ transpose $ map (padScenarios firstYear . scenarios) outputs'
    finances'  = map summation $ transpose $ map (padFinances firstYear  . finances ) outputs'
    performances' = computePerformanceAnalyses scenarioDefinition' $ zip scenarios' finances'
  in
    (
      outputs'
    , JSONOutputs
      {
        scenarioDefinition = scenarioDefinition'
      , stations           = stations'
      , scenarios          = scenarios'
      , finances           = finances'
      , analyses           = performances'
      }
    )


setHydrogenPrice :: [(Int, Double)] -> [(String, Capital, Scenario)] -> [(String, Capital, Scenario)]
setHydrogenPrice prices =
  map (\(f, s, t) -> (f, s, setHydrogenPrice' prices t))


setHydrogenPrice' :: [(Int, Double)] -> Scenario -> Scenario
setHydrogenPrice' prices scenario =
  scenario
  {
    hydrogenPrice =
      let
        price = fromMaybe (read "NaN") $ scenarioYear scenario `lookup` prices
      in
        if isNaN price
          then hydrogenPrice scenario
          else price
  }


findHydrogenPrice :: Double -> Outputs -> [(Int, Double)]
findHydrogenPrice offset outputs =
  let
    year = map scenarioYear $ scenarios outputs
    price = map hydrogenPrice $ scenarios outputs
    sales = map hydrogenSales $ scenarios outputs
    income = map (netIncome . cashFlowStatement) $ finances outputs
  in
    zipWith4 (\y p s i -> (y, p + offset - i / 365 / s)) year price sales income


padStations :: Int -> [Capital] -> [Capital]
padStations year ss = zipWith (\y s -> s {stationYear = y}) [year..] (replicate (stationYear (head ss) - year) zero) ++ ss


padScenarios :: Int -> [Scenario] -> [Scenario]
padScenarios year ss = zipWith (\y s -> s {scenarioYear = y}) [year..] (replicate (scenarioYear (head ss) - year) zero) ++ ss


padFinances :: Int -> [Finances] -> [Finances]
padFinances year fs = zipWith (\y s -> s {financesYear = y}) [year..] (replicate (financesYear (head fs) - year) zero) ++ fs


single :: Inputs -> [(Capital, Scenario)] -> Outputs
single parameters capitalScenarios =
  let
    (_scenarios, finances, performances) = solveConstrained' (scenario parameters) capitalScenarios
  in
    JSONOutputs
    {
      scenarioDefinition = scenario parameters
    , stations           = map fst capitalScenarios
    , scenarios          = map snd capitalScenarios
    , finances           = finances
    , analyses           = performances
    }


makeInputs :: Inputs -> FeedstockUsageCube -> EnergyPriceCube -> CarbonCreditCube -> StationUtilizationCube -> StationDetailCube -> [[(String, Capital, Scenario)]]
makeInputs parameters feedstockUsage energyPrices carbonCredits stationUtilization stationsDetail =
  let
    makeInputs' :: (Region, StationID, Maybe Int, Int, Double, Double, Double, Double, Double, Double, Double, Maybe Int) -> [FieldRec '[FRegion, FYear, FStationID, FNewCapitalCost, FNewInstallationCost, FNewCapitalIncentives, FNewProductionIncentives, FNewElectrolysisCapacity, FNewPipelineCapacity, FNewOnSiteSMRCapacity, FNewGH2TruckCapacity, FNewLH2TruckCapacity, FRenewableFraction]] -> [(String, Capital, Scenario)]
    makeInputs' (region', station', previousYear, totalStations', electrolysisCapacity', pipelineCapacity', onSiteSMRCapacity', gh2TruckCapacity', lh2TruckCapacity', renewableCapacity', totalCapital', firstYear') []           =
      let
        Just nextYear = (+1) <$> previousYear
        year = 2051
      in
        if nextYear >= year
          then
            []
          else
            makeInputs'
              (region', station', previousYear, totalStations', electrolysisCapacity', pipelineCapacity', onSiteSMRCapacity', gh2TruckCapacity', lh2TruckCapacity', renewableCapacity', totalCapital', firstYear')
              [
                  fRegion =: region'
              <+> fYear   =: nextYear
              <+> fStationID =: station'
              <+> fNewCapitalCost =: 0
              <+> fNewInstallationCost =: 0
              <+> fNewCapitalIncentives =: 0
              <+> fNewProductionIncentives =: 0
              <+> fNewElectrolysisCapacity =: 0
              <+> fNewPipelineCapacity =: 0
              <+> fNewOnSiteSMRCapacity =: 0
              <+> fNewGH2TruckCapacity =: 0
              <+> fNewLH2TruckCapacity =: 0
              <+> fRenewableFraction =: 0
              ]
    makeInputs' (_, _, previousYear, totalStations', electrolysisCapacity', pipelineCapacity', onSiteSMRCapacity', gh2TruckCapacity', lh2TruckCapacity', renewableCapacity', totalCapital', firstYear') (rec : recs) =
      let
        StationParameters{..} = station parameters
        region' = fRegion <: rec
        station' = fStationID <: rec 
        Just nextYear = (+1) <$> previousYear
        newElectrolysis = fNewElectrolysisCapacity <: rec
        newPipeline = fNewPipelineCapacity <: rec
        newOnSiteSMR = fNewOnSiteSMRCapacity <: rec
        newGH2Truck = fNewGH2TruckCapacity <: rec
        newLH2Truck = fNewLH2TruckCapacity <: rec
        newCapacity = newElectrolysis + newPipeline + newOnSiteSMR + newGH2Truck + newLH2Truck
        renewableCapacity = renewableCapacity' + newCapacity * (fRenewableFraction <: rec)
        renewableFraction = renewableCapacity / totalCapacity
        year = fYear <: rec
        year' = fromIntegral year
        firstYear = fromMaybe year firstYear'
        totalCapital = totalCapital' + capitalCost
        escalation = (1 + generalInflationRate (scenario parameters))**(fromIntegral $ year - referenceYear parameters)
        totalStations = totalStations' + newStations
        newStations = maybe 1 (const 0) firstYear'
        electrolysisCapacity = electrolysisCapacity' + newElectrolysis
        pipelineCapacity = pipelineCapacity' + newPipeline
        onSiteSMRCapacity = onSiteSMRCapacity' + newOnSiteSMR
        gh2TruckCapacity = gh2TruckCapacity' + newGH2Truck
        lh2TruckCapacity = lh2TruckCapacity' + newLH2Truck
        electricityUse = (
                           ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Electrolysis" <+> fFeedstockType =: FeedstockType "Electricity [kWh]"  )) * electrolysisCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Pipeline"     <+> fFeedstockType =: FeedstockType "Electricity [kWh]"  )) * pipelineCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "On-Site SMR"  <+> fFeedstockType =: FeedstockType "Electricity [kWh]"  )) * onSiteSMRCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Trucked GH2"  <+> fFeedstockType =: FeedstockType "Electricity [kWh]"  )) * gh2TruckCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Trucked LH2"  <+> fFeedstockType =: FeedstockType "Electricity [kWh]"  )) * lh2TruckCapacity
                         ) / totalCapacity
        naturalGasUse  = (
                           ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Electrolysis" <+> fFeedstockType =: FeedstockType "Natural Gas [mmBTU]")) * electrolysisCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Pipeline"     <+> fFeedstockType =: FeedstockType "Natural Gas [mmBTU]")) * pipelineCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "On-Site SMR"  <+> fFeedstockType =: FeedstockType "Natural Gas [mmBTU]")) * onSiteSMRCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Trucked GH2"  <+> fFeedstockType =: FeedstockType "Natural Gas [mmBTU]")) * gh2TruckCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Trucked LH2"  <+> fFeedstockType =: FeedstockType "Natural Gas [mmBTU]")) * lh2TruckCapacity
                         ) / totalCapacity
        hydrogenUse    = (
                           ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Electrolysis" <+> fFeedstockType =: FeedstockType "Hydrogen [kg]"      )) * electrolysisCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Pipeline"     <+> fFeedstockType =: FeedstockType "Hydrogen [kg]"      )) * pipelineCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "On-Site SMR"  <+> fFeedstockType =: FeedstockType "Hydrogen [kg]"      )) * onSiteSMRCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Trucked GH2"  <+> fFeedstockType =: FeedstockType "Hydrogen [kg]"      )) * gh2TruckCapacity
                         + ((fFeedstockUsage <:) $ feedstockUsage ! (fHydrogenSource =: HydrogenSource "Trucked LH2"  <+> fFeedstockType =: FeedstockType "Hydrogen [kg]"      )) * lh2TruckCapacity
                         ) / totalCapacity
        totalCapacity = electrolysisCapacity + pipelineCapacity + onSiteSMRCapacity + gh2TruckCapacity + lh2TruckCapacity
        capitalCost = fNewCapitalCost <: rec
        installationCost = fNewInstallationCost <: rec
        capitalGrant = fNewCapitalIncentives <: rec
        operatingGrant = fNewProductionIncentives <: rec
        electricity = ((fNonRenewablePrice <:) $ energyPrices ! (fYear =: year <+> fFeedstockType =: FeedstockType "Electricity [/kWh]"   )) * (1 - renewableFraction)
                    + ((fRenewablePrice    <:) $ energyPrices ! (fYear =: year <+> fFeedstockType =: FeedstockType "Electricity [/kWh]"   )) *      renewableFraction
        naturalGas  = ((fNonRenewablePrice <:) $ energyPrices ! (fYear =: year <+> fFeedstockType =: FeedstockType "Natural Gas [/mmBTU]" )) * (1 - renewableFraction)
                    + ((fRenewablePrice    <:) $ energyPrices ! (fYear =: year <+> fFeedstockType =: FeedstockType "Natural Gas [/mmBTU]" )) *      renewableFraction
        deliveredH2 = ((fNonRenewablePrice <:) $ energyPrices ! (fYear =: year <+> fFeedstockType =: FeedstockType "Hydrogen [/kg]"       )) * (1 - renewableFraction)
                    + ((fRenewablePrice    <:) $ energyPrices ! (fYear =: year <+> fFeedstockType =: FeedstockType "Hydrogen [/kg]"       )) *      renewableFraction
        retailH2    = ((fNonRenewablePrice <:) $ energyPrices ! (fYear =: year <+> fFeedstockType =: FeedstockType "Retail Hydrogen [/kg]")) * (1 - renewableFraction)
                    + ((fRenewablePrice    <:) $ energyPrices ! (fYear =: year <+> fFeedstockType =: FeedstockType "Retail Hydrogen [/kg]")) *      renewableFraction
        demand = (totalCapacity *) . maybe 0 (fStationUtilization <:) $ stationUtilization `evaluate` τ rec
        carbonCreditPerKg = (
                              ((fNonRenewableCredit <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "Electrolysis")) * electrolysisCapacity
                            + ((fNonRenewableCredit <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "Pipeline"    )) * pipelineCapacity
                            + ((fNonRenewableCredit <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "On-Site SMR" )) * onSiteSMRCapacity
                            + ((fNonRenewableCredit <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "Trucked GH2" )) * gh2TruckCapacity
                            + ((fNonRenewableCredit <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "Trucked LH2" )) * lh2TruckCapacity
                            ) / totalCapacity * (1 - renewableFraction)
                            +
                            (
                              ((fRenewableCredit    <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "Electrolysis")) * electrolysisCapacity
                            + ((fRenewableCredit    <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "Pipeline"    )) * pipelineCapacity
                            + ((fRenewableCredit    <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "On-Site SMR" )) * onSiteSMRCapacity
                            + ((fRenewableCredit    <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "Trucked GH2" )) * gh2TruckCapacity
                            + ((fRenewableCredit    <:) $ carbonCredits ! (fHydrogenSource =: HydrogenSource "Trucked LH2" )) * lh2TruckCapacity
                            ) / totalCapacity *      renewableFraction
      in
        if isNothing previousYear || nextYear == year
          then
            (
              show $ fStationID <: rec
            , Station {
                stationYear                            = year
              , stationTotal                           = totalStations
              , stationOperating                       = fromIntegral totalStations - fromIntegral newStations / 2
              , stationNew                             = newStations
              , stationElectricityUse                  = electricityUse
              , stationNaturalGasUse                   = naturalGasUse
              , stationDeliveredHydrogenUse            = hydrogenUse
              , stationCapacity                        = totalCapacity
              , stationCapitalCost                     = capitalCost
              , stationInstallationCost                = installationCost
              , stationIncidentalRevenue               = asFunction incidentalRevenueFunction year' + carbonCreditPerKg * 365.24 * demand * escalation
              , stationMaintenanceCost                 = totalCapital * maintenanceCostFraction * escalation
              , stationLicensingAndPermitting          = asFunction licensingAndPermittingFunction year'
              , stationRentOfLand                      = asFunction rentOfLandFunction year'
              , stationStaffing                        = staffing
              , stationLaborRate                       = asFunction laborRateFunction year'
              , stationSellingAndAdministrativeExpense = sellingAndAdministrativeExpense
              , stationCreditCardFeesRate              = creditCardFeesRate
              , stationSalesTaxRate                    = salesTaxRate
              , stationRoadTax                         = asFunction roadTaxFunction year'
              , stationPropertyTaxRate                 = propertyTaxRate
              , stationPropertyInsuranceRate           = propertyInsuranceRate
              }
            , Scenario
              {
                scenarioYear               = year
              , durationOfDebt             = year - firstYear + 1
              , newCapitalIncentives       = capitalGrant
              , newProductionIncentives    = operatingGrant
              , newFuelPrepayments         = 0
              , newCrowdFunding            = 0
              , newConsumerDiscounts       = 0
              , newCapitalExpenditures     = capitalCost
              , electricityCost            = electricity
              , naturalGasCost             = naturalGas
              , hydrogenCost               = deliveredH2
              , hydrogenPrice              = retailH2
              , stationUtilization         = demand / totalCapacity
              , hydrogenSales              = demand
              , fcevTotal                  = 0
              , fcevNew                    = 0
              , economyNet                 = nan
              , economyNew                 = nan
              , vmtTotal                   = nan
              }
            )
            : makeInputs' (region', station', Just year, totalStations, electrolysisCapacity, pipelineCapacity, onSiteSMRCapacity, gh2TruckCapacity, lh2TruckCapacity, renewableCapacity, totalCapital, Just firstYear) recs
          else
            makeInputs'
              (region', station', previousYear, totalStations', electrolysisCapacity', pipelineCapacity', onSiteSMRCapacity', gh2TruckCapacity', lh2TruckCapacity', renewableCapacity', totalCapital', Just firstYear)
              (
                (
                    fRegion =: fRegion <: rec
                <+> fYear   =: nextYear
                <+> fStationID =: fStationID <: rec
                <+> fNewCapitalCost =: 0
                <+> fNewInstallationCost =: 0
                <+> fNewCapitalIncentives =: 0
                <+> fNewProductionIncentives =: 0
                <+> fNewElectrolysisCapacity =: 0
                <+> fNewPipelineCapacity =: 0
                <+> fNewOnSiteSMRCapacity =: 0
                <+> fNewGH2TruckCapacity =: 0
                <+> fNewLH2TruckCapacity =: 0
                <+> fRenewableFraction =: 0
                )
                : rec : recs
              )
    isGeneric x
      | length x < 7 = False
      | otherwise    = take 7 x == "Generic"
  in
    map (replicateFirstYear . makeInputs' (undefined, undefined, Nothing, 0, 0, 0, 0, 0, 0, 0, 0, Nothing) . (\x -> unsafeInform ("Computing finances for \"" ++ show (fStationID <: head x) ++ "\" . . .") x))
      $ take (cohortFinish parameters - cohortStart parameters + 1)
      $ drop (cohortStart parameters - 1)
      $ sortBy (compare `on` (\recs -> (fYear <: head recs, isGeneric (show $ fStationID <: head recs), fStationID <: head recs)))
      $ groupBy ((==) `on` ((fRegion <:) &&& (fStationID <:)))
      $ sortBy (compare `on` (\rec -> (fRegion <: rec, fStationID <: rec, fYear <: rec)))
      $ toKnownRecords stationsDetail


-- FIXME: check station cost override ZERO needs to be computed if there is a new station.
-- FIXME: veto introduction until a specific year
-- FIXME: control stations, year of demand, year of first station automatically built

replicateFirstYear :: [(String, Capital, Scenario)] -> [(String, Capital, Scenario)]
replicateFirstYear ((r, c, s) : css) =
  (
    r
  , c
    {
      stationYear              = stationYear c - 1
    , stationOperating         = 0
    , stationMaintenanceCost   = 0
    , stationIncidentalRevenue = 0
    }
  , s
    {
      scenarioYear       = scenarioYear s - 1
    , durationOfDebt     = durationOfDebt s - 1
    , stationUtilization = 0
    , hydrogenSales      = 0
    }
  )
  :
  (
    r
  , c
    {
      stationNew              = 0
    , stationCapitalCost      = 0
    , stationInstallationCost = 0
    }
  , s
    {
      newCapitalIncentives   = 0
    , newCapitalExpenditures = 0
    }
  )
  : css
replicateFirstYear _ = undefined


data Outputs =
    JSONOutputs
    {
      scenarioDefinition :: ScenarioInputs
    , stations           :: [Capital]
    , scenarios          :: [Scenario]
    , finances           :: [Finances]
    , analyses           :: [PerformanceAnalysis]
    }
  | TSVOutputs
    {
      scenarioDefinitionTSV :: String
    , stationsTSV           :: String
    , scenariosTSV          :: String
    , financesTSV           :: String
    , analysesTSV           :: String
    }
    deriving (Generic, Read, Show)

instance FromJSON Outputs

instance ToJSON Outputs where
  toJSON = genericToJSON defaultOptions


makeTSVOutputs :: Outputs -> Outputs
makeTSVOutputs JSONOutputs{..} =
  TSVOutputs
    {
      scenarioDefinitionTSV = tabulationsT' [scenarioDefinition]
    , stationsTSV           = tabulationsT' stations
    , scenariosTSV          = tabulationsT' scenarios
    , financesTSV           = tabulationsT' finances
    , analysesTSV           = tabulationsT' analyses
    }
makeTSVOutputs o@TSVOutputs{} = o


dumpOutputs9 :: Outputs -> String
dumpOutputs9 outputs =
  let
    TSVOutputs{..} = makeTSVOutputs outputs
    basic =
      map (splitOn "\t")
      $ lines
      $ unlines
      [
        scenarioDefinitionTSV
      , ""
      , stationsTSV
      , ""
      , scenariosTSV
      , ""
      , financesTSV
      , ""
      , analysesTSV
      ]
    n = maximum $ map length basic
    pad x = take n $ x ++ repeat (if null x then [] else last x)
  in
    unlines
      $ map (intercalate "\t")
      $ transpose
      $ map pad
      basic


dumpOutputs' :: Outputs -> [[String]]
dumpOutputs' outputs =
  let
    TSVOutputs{..} = makeTSVOutputs outputs
  in
    map (splitOn "\t") $ lines $ unlines
      [
        scenarioDefinitionTSV
      , ""
      , stationsTSV
      , ""
      , scenariosTSV
      , ""
      , financesTSV
      , ""
      , analysesTSV
      ]
