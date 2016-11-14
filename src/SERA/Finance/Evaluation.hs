{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module SERA.Finance.Evaluation {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  makeScenarios
, computeFinances
) where


import Control.Arrow ((***), second)
import Control.Monad (liftM2)
import Data.List.Util (regroup)
import Data.Maybe (fromMaybe)
import Data.Tuple.Util (fst3, snd3, trd3)
import Math.Series (deltas)
import SERA.Constants (days_per_year, gge_per_J, kgH2_per_J)
import SERA.Energy (EnergyCosts)
import SERA.Configuration.RiskInputs (RiskInputs)
import SERA.Configuration.ScenarioInputs (ScenarioInputs(..))
import SERA.Finance.Analysis.Finances (Finances(..))
import SERA.Finance.Analysis.PerformanceAnalysis (PerformanceAnalysis)
import SERA.Finance.Capital (Capital(..))
import SERA.Finance.Demand (Demand(Demand))
import SERA.Finance.Project (Project)
import SERA.Finance.Risks (applyRisks')
import SERA.Finance.Scenario (Scenario(Scenario), zeroScenario)
import SERA.Finance.Solution (solveConstrained')
import SERA.Util.Time (Year(..))

import qualified SERA.Finance.Demand as D (Demand(..))
import qualified SERA.Finance.Scenario as S (Scenario(..))


makeScenarios :: Int -> ScenarioInputs -> EnergyCosts -> [(Capital, Demand)] -> [(Capital, Scenario)]
makeScenarios = (((computeNewVehicles .) .) .) . ((map .) .) . makeScenario


computeNewVehicles :: [(Capital, Scenario)] -> [(Capital, Scenario)]
computeNewVehicles stations =
  let
    vehicles :: [Int]
    vehicles = deltas $ map (S.fcevTotal . snd) stations
    inject :: (Capital, Scenario) -> Int -> (Capital, Scenario)
    inject (station, scenario@Scenario{..}) new = (station, scenario {S.fcevNew = new})
  in
    zipWith inject stations vehicles


makeScenario :: Int -> ScenarioInputs -> EnergyCosts -> (Capital, Demand) -> (Capital, Scenario)
makeScenario firstYear ScenarioInputs{..} energyCosts (station@Station{..}, Demand{..}) =
  let
    scenario = zeroScenario energyCosts stationYear
  in
    (
      station
    , scenario
      {
        S.durationOfDebt         = stationYear - firstYear
      , S.newCapitalExpenditures = stationCapitalCost + stationInstallationCost
      , S.stationUtilization     = hydrogenDemand / stationCapacity
      , S.hydrogenSales          = hydrogenDemand
      , S.fcevTotal              = round fcevTotal
      , S.vmtTotal               = S.economyNet scenario * gge_per_J / kgH2_per_J * hydrogenDemand * days_per_year / fcevTotal
      }
    )


makeIncentives :: Int -> ScenarioInputs -> [(Capital, Scenario)] -> [(Capital, Scenario)]
makeIncentives firstYear ScenarioInputs{..} stationScenarios =
  let
    capitalIncentives :: [(Int, Double)]
    capitalIncentives =
      [
        (
          stationYear - offset
        , if stationYear - offset < firstYear + durationOfCapitalIncentive
            then fromIntegral stationNew * capitalIncentive
            else 0
        )
      |
        (Station{..}, Scenario{..}) <- stationScenarios
      , let offset = 1
      ]
    productionIncentives :: [(Int, Double)]
    productionIncentives = (map . second) sum $ regroup $ concat
      [
        [
          (
            stationYear + offset + dYear
          , if stationYear - offset < firstYear + durationOfProductionIncentive
              then
                fromIntegral stationNew
                  * (productionIncentiveStart - fromIntegral dYear * productionIncentiveAnnualDecrement)
                  * f0
              else 0
          )
        ,
          (
            stationYear + offset + 1 + dYear
          , if stationYear + offset + 1 < firstYear + durationOfProductionIncentive
              then
                fromIntegral stationNew
                  * (productionIncentiveStart - fromIntegral dYear * productionIncentiveAnnualDecrement)
                  * f1
              else 0
          )
        ]
      |
        (Station{..}, Scenario{..}) <- stationScenarios
      , dYear <-
          if productionIncentiveAnnualDecrement == 0
            then [0..length stationScenarios]
            else [0..(floor $ productionIncentiveStart / productionIncentiveAnnualDecrement)]
      , let
          offset = floor installationTime
          f0 = installationTime - fromIntegral offset
          f1 = 1 - f0
      ]
    fuelPrepayments :: [(Int, Double)]
    fuelPrepayments = (map . second) sum $ regroup
      -- FIXME: account for installation
      [
        (
          stationYear + dYear
        , if stationYear < firstYear + durationOfFuelPrepayments
            then
              fromIntegral stationNew
                * (
                    annualFractionOfFuelPrepaid
                    +
                    (
                     if dYear == 0
                        then multiYearFractionOfFuelPrepaid * fromIntegral lengthOfMultiYearPrepayment
                        else 0
                    )
                  )
                * hydrogenPrice
                * 365 * stationUtilization * stationCapacity
            else 0
        )
      |
        (Station{..}, Scenario{..}) <- stationScenarios
      , dYear <-
          if lengthOfMultiYearPrepayment == 0
            then [0]
            else [0..(lengthOfMultiYearPrepayment - 1)]
      ]
    consumerDiscounts :: [(Int, Double)]
    consumerDiscounts = (map . second) sum $ regroup $
      -- FIXME: account for installation
      [
        (
          stationYear + dYear
        , if stationYear < firstYear + durationOfFuelPrepayments
            then
              - fromIntegral stationNew
                * (
                    annualFractionOfFuelPrepaid
                    +
                    multiYearFractionOfFuelPrepaid
                  )
                * hydrogenPrice * 365 * hydrogenSales
            else 0
        )
      |
        (Station{..}, Scenario{..}) <- stationScenarios
      , dYear <-
          if lengthOfMultiYearPrepayment == 0
            then [0]
            else [0..(lengthOfMultiYearPrepayment - 1)]
      ]
      ++
      [
        (
          stationYear + dYear
        , if fcevTotal > 0
            then - hydrogenPrice
              * consumerDiscountOnHydrogen
              * 365 * hydrogenSales / fromIntegral fcevTotal
              * minimum [fromIntegral fcevNew, limitOnConsumerPayments * fromIntegral stationNew]
            else 0
        )
      |
        (Station{..}, Scenario{..}) <- stationScenarios
      , dYear <- [0..length stationScenarios]
      ]
    crowdFunding :: [(Int, Double)]
    crowdFunding = (map . second) sum $ regroup
      -- FIXME: account for installation
      [
        (
          stationYear
        , oneTimeConsumerPayment
            * minimum [fromIntegral fcevNew, limitOnConsumerPayments * fromIntegral stationNew]
        )
      |
        (Station{..}, Scenario{..}) <- stationScenarios
      ]
  in
    [
      (
        station
      , scenario
          {
            S.newCapitalIncentives    = fromMaybe 0 $ year `lookup` capitalIncentives
          , S.newProductionIncentives = fromMaybe 0 $ year `lookup` productionIncentives
          , S.newFuelPrepayments      = fromMaybe 0 $ year `lookup` fuelPrepayments
          , S.newCrowdFunding         = fromMaybe 0 $ year `lookup` crowdFunding
          , S.newConsumerDiscounts    = fromMaybe 0 $ year `lookup` consumerDiscounts
          }
      )
    |
      (station, scenario) <- stationScenarios
    , let year = S.scenarioYear scenario
    ]


computeFinances :: Maybe RiskInputs -> ScenarioInputs -> EnergyCosts -> (Project, [(Year, Capital, Demand)]) -> (Project, Maybe RiskInputs, ScenarioInputs, [Capital], [Scenario], [Finances], [PerformanceAnalysis])
computeFinances risks parameters energyCosts (region, stations) =
  let
    parameters' = applyRisks' risks parameters
    firstYear = unYear $ fst3 $ head stations
    stations' :: [(Capital, Demand)]
    stations' = map (liftM2 (,) snd3 trd3) stations
    scenarios :: [(Capital, Scenario)]
    scenarios = map (applyRisks' risks *** applyRisks' risks) $ makeScenarios firstYear parameters' energyCosts stations'
    scenarios' :: [Scenario]
    finances :: [Finances]
    overall :: [PerformanceAnalysis]
    (scenarios', finances, overall) =
      solveConstrained' parameters'
        $ makeIncentives firstYear parameters' scenarios
    -- FIXME: in principle, we need to iterate and converge because hydrogen price is changing
  in
    (region, risks, parameters', map fst scenarios, scenarios', finances, overall)
