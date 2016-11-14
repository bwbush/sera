{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Service.Finance (
  financeMain
) where


import Control.Arrow (second)
import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (zero)
import Data.Function (on)
import Data.List (groupBy, transpose, zipWith4)
import Data.List.Split (splitOn)
import Data.Table (Tabulatable(..))
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFile)
import GHC.Generics (Generic)
import SERA.Configuration.ScenarioInputs (ScenarioInputs(..))
import SERA.Energy (EnergyCosts(..))
import SERA.Finance.Analysis (computePerformanceAnalyses)
import SERA.Finance.Analysis.CashFlowStatement (CashFlowStatement(..))
import SERA.Finance.Analysis.Finances (Finances(..))
import SERA.Finance.Analysis.PerformanceAnalysis (PerformanceAnalysis)
import SERA.Finance.Capital (Capital(..), Operations(..), costStation)
import SERA.Refueling.FCVSE.Cost.NREL56412 (rentCost, totalFixedOperatingCost)
import SERA.Finance.IO.Xlsx (formatResultsAsFile)
import SERA.Finance.Scenario (Scenario(..))
import SERA.Finance.Solution (solveConstrained')
import SERA.Util.Summarization (summation)
import System.Environment (getArgs)


data Inputs =
  Inputs
    {
      scenario       :: ScenarioInputs
    , station        :: Capital
    , operations     :: Operations
    , energyCosts    :: EnergyCosts
    }
    deriving (Generic, Read, Show)

instance FromJSON Inputs

instance ToJSON Inputs where
  toJSON = genericToJSON defaultOptions


financeMain :: IO ()
financeMain =
  do
    [defaultFile, inputFile, outputFile, summaryFile, targetMargin] <- getArgs
    Just parameters <- decodeFile defaultFile
    inputs <- map (splitOn "\t") . tail . lines <$> readFile inputFile
    let
      inputs' =
        groupBy ((==) `on` head)
          $ filter ((/= 0) . (read :: String -> Int) . (!! 3)) inputs
      ids = map (head . head) inputs'
      prepared = map (prepareInputs parameters) inputs'
      prepared' = case read targetMargin of
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
    sequence_
      [
        formatResultsAsFile outputFile' $ dumpOutputs' output
      |
        (idx, output) <- zip ids outputs
      , let outputFile' = "stations/finances-" ++ idx ++ ".xlsx"
      ]
    formatResultsAsFile outputFile $ dumpOutputs' allOutputs
    writeFile summaryFile $ dumpOutputs9 allOutputs


multiple :: Inputs -> [[(Capital, Scenario)]] -> ([Outputs], Outputs)
multiple parameters capitalScenarios =
  let
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


setHydrogenPrice :: [(Int, Double)] -> [(Capital, Scenario)] -> [(Capital, Scenario)]
setHydrogenPrice prices =
  map (second $ setHydrogenPrice' prices)


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


prepareInputs :: Inputs -> [[String]] -> [(Capital, Scenario)]
prepareInputs parameters inputs =
  let
    firstYear :: Int
    firstYear = read $ head inputs !! 2
    capitalScenarios :: [(Capital, Scenario)]
    capitalScenarios =
      [
        (
          (\c@Station{..} ->
            let
              totalStations' = read totalStations 
              averageCapacity = read totalCapacity / totalStations'
              escalation = 1.019**(fromIntegral stationYear - 2013)
            in
              c {
                  stationLicensingAndPermitting = read totalStations * stationLicensingAndPermitting
                , stationMaintenanceCost        = escalation * totalStations' * (totalFixedOperatingCost averageCapacity - rentCost averageCapacity)
                , stationRentOfLand             = escalation * totalStations' * rentCost averageCapacity
                }
          ) $ costStation 0 (operations parameters) (read year) $ (station parameters)
          {
            stationYear                            = read year
          , stationTotal                           = read totalStations
          , stationOperating                       = read totalStations - read newStations / 2
          , stationNew                             = read newStations
          , stationElectricityUse                  = read electricityUse
          , stationNaturalGasUse                   = read naturalGasUse
          , stationDeliveredHydrogenUse            = read hydrogenUse
          , stationCapacity                        = read totalCapacity
          , stationCapitalCost                     = read capitalCost
          , stationInstallationCost                = read installationCost
          , stationIncidentalRevenue               = read incidentalRevenue
--        , stationMaintenanceCost                 = read "NaN"
--        , stationLicensingAndPermitting          = read "NaN"
--        , stationRentOfLand                      = read "NaN"
--        , stationStaffing                        = read "NaN"
--        , stationLaborRate                       = read "NaN"
--        , stationSellingAndAdministrativeExpense = read "NaN"
--        , stationCreditCardFeesRate              = read "NaN"
--        , stationSalesTaxRate                    = read "NaN"
--        , stationRoadTax                         = read "NaN"
--        , stationPropertyTaxRate                 = read "NaN"
--        , stationPropertyInsuranceRate           = read "NaN"
          }
        , Scenario
          {
            scenarioYear               = read year
          , durationOfDebt             = read year - firstYear + 1
          , newCapitalIncentives       = read capitalGrant
          , newProductionIncentives    = read operatingGrant
          , newFuelPrepayments         = 0
          , newCrowdFunding            = 0
          , newConsumerDiscounts       = 0
          , newCapitalExpenditures     = read capitalCost
          , electricityCost            = read electricity
          , naturalGasCost             = read naturalGas
          , hydrogenCost               = read deliveredH2
          , hydrogenPrice              = read retailH2
          , stationUtilization         = read demand / read totalCapacity
          , hydrogenSales              = read demand
          , fcevTotal                  = 0
          , fcevNew                    = 0
          , economyNet                 = read "NaN"
          , economyNew                 = read "NaN"
          , vmtTotal                   = read "NaN"
          }
        )
      |
        [_regionId, _regionName, year, totalStations, newStations, electricityUse, naturalGasUse, hydrogenUse, totalCapacity, capitalCost, installationCost, incidentalRevenue, capitalGrant, operatingGrant, electricity, naturalGas, deliveredH2, retailH2, demand] <- inputs
      , (read totalStations :: Int) > 0
      ]
  in
    accumulateMaintenanceCosts 0
      $ extendTo2050
      $ replicateFirstYear capitalScenarios


accumulateMaintenanceCosts :: Double -> [(Capital, Scenario)] -> [(Capital, Scenario)]
accumulateMaintenanceCosts _        [] = []
accumulateMaintenanceCosts previous ((c, s) : css) =
  (c {stationMaintenanceCost = alpha * stationMaintenanceCost c + beta * 0.05 * cumulativeCapitalCost}, s)
    : accumulateMaintenanceCosts cumulativeCapitalCost css
    where
      alpha = maximum [0, minimum [1, (2025 - fromIntegral (stationYear c)) / (2025 - 2015)]]
      beta = 1 - alpha
      cumulativeCapitalCost = 1.019 * previous + stationCapitalCost c


replicateFirstYear :: [(Capital, Scenario)] -> [(Capital, Scenario)]
replicateFirstYear ((c, s) : css) =
  (
    c
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
    c
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


extendTo2050 :: [(Capital, Scenario)] -> [(Capital, Scenario)]
extendTo2050 [] = []
extendTo2050 [cs@(c, s)] =
  cs :
    if stationYear (fst cs) >= 2050
      then []
      else
        extendTo2050
        [
          (
            c
            {
              stationYear                   = 1 + stationYear c
            , stationOperating              = fromIntegral $ stationTotal c
            , stationNew                    = 0
            , stationCapitalCost            = 0
            , stationInstallationCost       = 0
            , stationIncidentalRevenue      = 1.019 * stationIncidentalRevenue c
            , stationMaintenanceCost        = 1.019 * stationMaintenanceCost c
            , stationLicensingAndPermitting = 1.019 * stationLicensingAndPermitting c
            , stationRentOfLand             = 1.019 * stationRentOfLand c
            , stationLaborRate              = 1.019 * stationLaborRate c
            }
          , s
            {
              scenarioYear            = 1 + scenarioYear s
            , durationOfDebt          = 1 + durationOfDebt s
            , newCapitalIncentives    = 0
            , newProductionIncentives = 0
            , newCapitalExpenditures  = 0
            , electricityCost         = 1.019 * electricityCost s
            , naturalGasCost          = 1.019 * naturalGasCost s
            , hydrogenCost            = 1.019 * hydrogenCost s
            , hydrogenPrice           = 1.019 * hydrogenPrice s
            , stationUtilization      = (0.75 + stationUtilization s) / 2
            , hydrogenSales           = (0.75 * stationCapacity c + hydrogenSales s) / 2
            }
          )
        ]
extendTo2050 (cs : css) = cs : extendTo2050 css


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
  in
    unlines
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
