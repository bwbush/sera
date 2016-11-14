{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Solution {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  solveConstrained
, solveConstrained'
) where


import Control.Arrow (second)
import Control.Monad (unless)
import Data.Default.Util (nan)
import Data.List (inits)
import Math.Roots (bracketOutward)
import Math.Roots.Bisection (findRoot)
import SERA.Configuration.ScenarioInputs (ScenarioInputs(..))
import SERA.Finance.Analysis (computeAllFinances, computePerformanceAnalyses, zipStationsWithScenarios) 
import SERA.Finance.Analysis.Finances (Finances(..))
import SERA.Finance.Analysis.PerformanceAnalysis (PerformanceAnalysis(..))
import SERA.Finance.Capital (Capital, Operations)
import SERA.Finance.Scenario (Scenario(..))


solveConstrained :: ScenarioInputs -> Operations -> Capital -> [Scenario] -> ([Scenario], [Finances], [PerformanceAnalysis])
solveConstrained = (. zipStationsWithScenarios) . (.) . (.) . solveConstrained'


solveConstrained' :: ScenarioInputs -> [(Capital, Scenario)] -> ([Scenario], [Finances], [PerformanceAnalysis])
solveConstrained' parameters stationScenarios =
  let
    scenarios = map snd stationScenarios
    finances = computeAllFinances parameters stationScenarios
    analyses = computePerformanceAnalyses parameters $ zip scenarios finances
    multipliers = map (findBreakEvenHydrogenPriceMultiplier parameters) $ tail $ inits stationScenarios
    analyses' = zipWith (\a m -> a {breakEvenHydrogenPrice = m * breakEvenHydrogenPrice a}) analyses multipliers
  in
    (scenarios, finances, analyses')


solveConstrained'' :: ScenarioInputs -> [(Capital, Scenario)] -> ([Scenario], [Finances], [PerformanceAnalysis])
solveConstrained'' parameters stationScenarios =
  let
    scenarios = map snd stationScenarios
    finances = computeAllFinances parameters stationScenarios
    analyses = computePerformanceAnalyses parameters $ zip scenarios finances
  in
    (scenarios, finances, analyses)


findBreakEvenHydrogenPriceMultiplier :: ScenarioInputs -> [(Capital, Scenario)] -> Double
findBreakEvenHydrogenPriceMultiplier =
  if True
    then findBreakEvenHydrogenPriceMultiplier'
    else findBreakEvenHydrogenPriceMultiplier''


findBreakEvenHydrogenPriceMultiplier' :: ScenarioInputs -> [(Capital, Scenario)] -> Double
findBreakEvenHydrogenPriceMultiplier' parameters stationScenarios =
  -- FIXME if Either were MonadPlus we could use guard.
  either (const nan) id $ do
    _ <- unless (hydrogenSales (snd $ last stationScenarios) > 0) $ Left "never any hydrogen sales"
    let
      firstPrice = hydrogenPrice $ snd $ head stationScenarios
      f = snd . npvForHydrogenPrice parameters stationScenarios
    ps <- bracketOutward 20 f (0.1, 1000)
    p <- findRoot 100 1e-3 f ps
    return $ p / firstPrice


findBreakEvenHydrogenPriceMultiplier'' :: ScenarioInputs -> [(Capital, Scenario)] -> Double
findBreakEvenHydrogenPriceMultiplier'' parameters stationScenarios =
  if hydrogenSales (snd $ last stationScenarios) > 0
    then
      fst
        $ head
        $ dropWhile ((< 0) . snd)
        $  map (npvForHydrogenPrice parameters stationScenarios)
        [0.005,0.010..]
    else nan


npvForHydrogenPrice :: ScenarioInputs -> [(Capital, Scenario)] -> Double -> (Double, Double)
npvForHydrogenPrice parameters@ScenarioInputs{..} stationScenarios price =
  let
    firstPrice = hydrogenPrice $ snd $ head stationScenarios
    multiplier = price / firstPrice
    stationScenarios' = map (second $ \s -> s {hydrogenPrice = multiplier * hydrogenPrice s}) stationScenarios
    (_, _, analysis) = solveConstrained'' parameters stationScenarios'
  in
    (multiplier, netPresentValue $ last analysis)
