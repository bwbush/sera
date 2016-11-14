{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  computeAllFinances
, computePerformanceAnalyses
, zipStationsWithScenarios
) where


import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Default.Util (Zero(..), nan)
import Data.Function.Excel (ipmt, ppmt)
import Data.List (inits)
import Data.Maybe (fromMaybe)
import SERA.Configuration.ScenarioInputs (DebtType(..), ScenarioInputs(..))
import SERA.Constants (days_per_year)
import SERA.Finance.Analysis.Assets as Assets (Assets(..))
import SERA.Finance.Analysis.BalanceSheet as BalanceSheet (BalanceSheet(..))
import SERA.Finance.Analysis.CashFlowStatement as CashFlowStatement (CashFlowStatement(..))
import SERA.Finance.Analysis.Equity as Equity (Equity(..))
import SERA.Finance.Analysis.Expenses as Expenses (Expenses(..))
import SERA.Finance.Analysis.FeedstockUtilityExpenses (FeedstockUtilityExpenses(..))
import SERA.Finance.Analysis.Finances as Finances (Finances(..))
import SERA.Finance.Analysis.IncomeStatement as IncomeStatement (IncomeStatement(..))
import SERA.Finance.Analysis.Liabilities as Liabilities (Liabilities(..))
import SERA.Finance.Analysis.OperatingExpenses (OperatingExpenses(..))
import SERA.Finance.Analysis.PerformanceAnalysis (PerformanceAnalysis(..))
import SERA.Finance.Analysis.Revenues as Revenues (Revenues(..))
import SERA.Finance.Analysis.Taxation as Taxation (Taxation(..), unknownTaxation)
import SERA.Finance.Capital (Capital(..), Operations, ageStationTo)
import SERA.Finance.IRS946 (depreciate)
import SERA.Finance.Scenario as Scenario (Scenario(..))
import SERA.Util.Summarization (Totalable(..), sumEach)

import qualified Data.Function.Finance as F (netPresentValue)
import qualified SERA.Finance.Analysis.Finances as Finances (totalExpenses, totalRevenues)


compatibilityWithVersion146 :: Bool
compatibilityWithVersion146 = True


proRataInitialPropertyExpenses :: Bool
proRataInitialPropertyExpenses = compatibilityWithVersion146


matchExcelIrr :: Bool
matchExcelIrr = True


irr :: [Double] -> Double
irr xs =
  let
    r0 :: Double
    r0 = 0.1
    maxIterationCount :: Int
    maxIterationCount = 20
    absoluteAccuracy :: Double
    absoluteAccuracy = 1e-7
    f :: Double -> Double
    f r =
      let
        y = sum  $ zipWith (\x k ->       x / (1 + r)** k     ) xs [0..]
        y' = sum $ zipWith (\x k -> - k * x / (1 + r)**(k + 1)) xs [0..]
      in
        r - y / y'
    rs :: [Double]
    rs = take (maxIterationCount + 1) $ iterate f r0
    g :: (Double, Bool) -> Double -> (Double, Bool)
    g ro@(r, okay) r' =
      if okay
        then ro
      else (r', abs (r - r') <= absoluteAccuracy)
    (r1, found) = foldl g (nan, False) rs
  in
    if found
      then r1
      else if matchExcelIrr
        then nan
        else irr' xs


irr' :: [Double] -> Double
irr' xs =
  fromMaybe nan $ do
    guard $ minimum xs * maximum xs < 0
    let
      f = flip F.netPresentValue xs
      (r, (fm, fp)) =
         head
           $ dropWhile ((> 0) . uncurry (*) . snd)
           $ map (id &&& (f . negate &&& f))
             [0.0001,0.0002..]
    return $
      if abs fm > abs fp
        then r
        else -r
      

computeAllFinances :: ScenarioInputs -> [(Capital, Scenario)] -> [Finances]
computeAllFinances parameters =
  tail . reverse . foldl computeNextFinances [zero] 
    where computeNextFinances finances0 (stationCharacteristics, scenario) = computeFinances parameters stationCharacteristics (head finances0) scenario : finances0


zipStationsWithScenarios :: Operations -> Capital -> [Scenario] -> [(Capital, Scenario)]
zipStationsWithScenarios stationOperations stationCharacteristics scenarios =
  let
    stationCharacteristics' = ageStationTo stationOperations stationCharacteristics $ scenarioYear (last scenarios)
  in zip stationCharacteristics' scenarios


computeFinances :: ScenarioInputs -> Capital -> Finances -> Scenario -> Finances
computeFinances parameters stationCharacteristics finances0 scenario@Scenario{..} =
  let

    financesYear = scenarioYear

    incomeStatement0 = Finances.incomeStatement finances0
    balanceSheet0 = Finances.balanceSheet finances0

    incomeStatement'' = computeBasicIncomeStatement parameters stationCharacteristics scenario balanceSheet0
    cashFlowStatement'' = computeCashFlowStatement parameters balanceSheet0 stationCharacteristics scenario incomeStatement''
    balanceSheet'' = computeBalanceSheet balanceSheet0 scenario cashFlowStatement'' unknownTaxation

    incomeStatement' = computeIncomeStatement parameters balanceSheet0 incomeStatement0 stationCharacteristics scenario balanceSheet''
    taxation' = Expenses.taxation $ expenses incomeStatement'
    cashFlowStatement' = computeCashFlowStatement parameters balanceSheet0 stationCharacteristics scenario incomeStatement'
    balanceSheet' = computeBalanceSheet balanceSheet0 scenario cashFlowStatement' taxation'

    incomeStatement = computeIncomeStatement parameters balanceSheet0 incomeStatement0 stationCharacteristics scenario balanceSheet'
    taxation = Expenses.taxation $ expenses incomeStatement
    cashFlowStatement = computeCashFlowStatement parameters balanceSheet0 stationCharacteristics scenario incomeStatement
    balanceSheet = computeBalanceSheet balanceSheet0 scenario cashFlowStatement taxation

  in
    Finances{..}


computeBasicIncomeStatement :: ScenarioInputs -> Capital -> Scenario -> BalanceSheet-> IncomeStatement
computeBasicIncomeStatement parameters stationCharacteristics scenario balanceSheet =
  let
    revenues = computeRevenues stationCharacteristics scenario
    expenses = computeBasicExpenses parameters stationCharacteristics scenario revenues balanceSheet
  in
    IncomeStatement{..}


computeIncomeStatement :: ScenarioInputs -> BalanceSheet -> IncomeStatement -> Capital -> Scenario -> BalanceSheet-> IncomeStatement
computeIncomeStatement parameters balanceSheet0 incomeStatement0 stationCharacteristics scenario balanceSheet =
  let
    revenues = computeRevenues stationCharacteristics scenario
    expenses0 = IncomeStatement.expenses incomeStatement0
    expenses = computeExpenses parameters stationCharacteristics scenario balanceSheet0 expenses0 revenues balanceSheet
  in
    IncomeStatement{..}


computeRevenues :: Capital -> Scenario -> Revenues
computeRevenues Station{..} Scenario{..} =
  let
    proRata =
      if stationTotal > 0
        then stationOperating / fromIntegral stationTotal
        else 0
    annualHydrogenConsumption = days_per_year * stationCapacity * stationUtilization
    sales = hydrogenPrice * annualHydrogenConsumption
    productionIncentive = newProductionIncentives
    incidentalRevenue = stationIncidentalRevenue * proRata
    fuelPrepayments = newFuelPrepayments
    consumerDiscounts = newConsumerDiscounts
    creditCardFees = - sales * stationCreditCardFeesRate
    salesTaxes = - sales * stationSalesTaxRate
    roadTaxes = - stationRoadTax * annualHydrogenConsumption
  in
    Revenues{..}


computeBasicExpenses :: ScenarioInputs -> Capital -> Scenario -> Revenues -> BalanceSheet -> Expenses
computeBasicExpenses parameters stationCharacteristics scenario revenues BalanceSheet{..} =
  let
    operatingExpenses = computeOperatingExpenses parameters scenario stationCharacteristics revenues assets 
    taxation = unknownTaxation
  in
    Expenses{..}


computeExpenses :: ScenarioInputs -> Capital -> Scenario -> BalanceSheet -> Expenses -> Revenues -> BalanceSheet -> Expenses
computeExpenses parameters stationCharacteristics scenario balanceSheet0 expenses0 revenues BalanceSheet{..} =
  let
    operatingExpenses = computeOperatingExpenses parameters scenario stationCharacteristics revenues assets 
    taxation0 = Expenses.taxation expenses0
    taxation = computeTaxation parameters balanceSheet0 stationCharacteristics scenario taxation0 revenues operatingExpenses liabilities
  in
    Expenses{..}


computeOperatingExpenses :: ScenarioInputs -> Scenario -> Capital -> Revenues -> Assets -> OperatingExpenses
computeOperatingExpenses ScenarioInputs{..} scenario@Scenario{..} stationCharacteristics@Station{..} Revenues{..} Assets{..} =
  let
    proRata =
      if stationTotal > 0
        then stationOperating / fromIntegral stationTotal
        else 0
    proRata' =
      if durationOfDebt > 0
        then if proRataInitialPropertyExpenses || fromIntegral durationOfDebt > installationTime + equipmentLife
          then proRata
          else 1
        else 0
    feedstockAndUtilities = computeFeedstockUtilityExpenses scenario stationCharacteristics
    labor = stationStaffing * stationLaborRate * proRata
    maintenance = stationMaintenanceCost * proRata
    rent = stationRentOfLand * proRata'
    propertyInsurance = stationPropertyInsuranceRate * netPPE * proRata'
    licensingAndPermitting = stationLicensingAndPermitting * proRata'
    sellingAndAdministrative = sales * stationSellingAndAdministrativeExpense
  in
    OperatingExpenses{..}


computeFeedstockUtilityExpenses :: Scenario -> Capital -> FeedstockUtilityExpenses
computeFeedstockUtilityExpenses Scenario{..} Station{..} =
  let
    annualHydrogenConsumption = days_per_year * stationCapacity * stationUtilization
    electricity = electricityCost * annualHydrogenConsumption * stationElectricityUse
    naturalGas = naturalGasCost * annualHydrogenConsumption * stationNaturalGasUse
    deliveredHydrogen = hydrogenCost * annualHydrogenConsumption * stationDeliveredHydrogenUse
  in
    FeedstockUtilityExpenses{..}


computeTaxation :: ScenarioInputs -> BalanceSheet -> Capital -> Scenario -> Taxation -> Revenues -> OperatingExpenses -> Liabilities-> Taxation
computeTaxation ScenarioInputs{..} balanceSheet0 Station{..} Scenario{..} taxation0 revenues@Revenues{..} operatingExpenses Liabilities{..} =
  let
    accumulatedDebt0 = Liabilities.accumulatedDebt $ BalanceSheet.liabilities balanceSheet0
    earningsBeforeInterestTaxesAndDepreciation = total revenues - total operatingExpenses
    interestOnOutstandingDebt =
      if durationOfDebt > 0
        then case debtType of
          OneTimeLoan   -> if durationOfDebt <= loanPeriod
                             then - sum [ipmt (debtInterestRate / 12) per ((loanPeriod - durationOfDebt + 1) * 12) accumulatedDebt0 Nothing Nothing | per <- [1..12]]
                             else 0
          RevolvingDebt -> accumulatedDebt0 * debtInterestRate
                             * if durationOfDebt == ceiling (installationTime + equipmentLife)
                                 then stationOperating / fromIntegral stationTotal
                                 else 1
        else 0
    depreciable =
      stationCapitalCost
        + (if installationCostsDepreciable then stationInstallationCost else 0)
        - (if capitalIncentiveDepreciable then 0 else newCapitalIncentives)
    yearsBeforeDepreciation = ceiling (installationTime + 1 / 12) - 1
    quarterPlacedInService =
      ceiling (
        (
          installationTime + 0.5 / 12
          -
          ((fromIntegral :: Int -> Double) . floor) (installationTime + 0.5 / 12)
        ) * 4
      )
    newEquipmentDepreciation =
      map (depreciable *)
        $ replicate (1 + yearsBeforeDepreciation) 0 ++ depreciate quarterPlacedInService depreciationPeriod
    equipmentDepreciation0 = Taxation.equipmentDepreciation taxation0
    equipmentDepreciation = sumEach [newEquipmentDepreciation, tail equipmentDepreciation0]
    taxableIncome = 
      earningsBeforeInterestTaxesAndDepreciation
      - interestOnOutstandingDebt
      - head equipmentDepreciation
      - if operatingIncentivesTaxable then 0 else productionIncentive
    taxesBeforeDeferment = taxableIncome * totalTaxRate
    taxesBeforeDeferment0 = Taxation.taxesBeforeDeferment taxation0
    tlcf0 0 = minimum [taxesBeforeDeferment0, 0]
    tlcf0 n = Taxation.taxLossCarryForward taxation0 !! (n - 1)
    tlcf n
      | n == allowableTaxLossCarryForward = minimum [tlcf0 (n - 1) + maximum [taxesBeforeDeferment, 0], 0]
      | otherwise                         = if sum [tlcf m | m <- [(n+1)..allowableTaxLossCarryForward]] == 0
                                              then
                                                minimum
                                                  [
                                                    sum [tlcf0 m | m <- [(n-1)..(allowableTaxLossCarryForward-1)]]
                                                      + maximum [taxesBeforeDeferment, 0]
                                                  , 0
                                                  ]
                                              else tlcf0 (n - 1)
    taxLossCarryForward = [tlcf m | m <- [1..allowableTaxLossCarryForward]]
    taxesDue
      | taxLossesMonetized               = taxesBeforeDeferment
      | allowableTaxLossCarryForward > 0 = maximum
                                             [
                                               0
                                             , maximum [taxesBeforeDeferment, 0]
                                                 + sum
                                                     [
                                                       tlcf0 n
                                                     |
                                                       n <- [0 .. (allowableTaxLossCarryForward-1)]
                                                     , tlcf0 n < 0
                                                     ]
                                             ]
      | otherwise                        = maximum [taxesBeforeDeferment, 0]
  in
    Taxation{..}


computeCashFlowStatement :: ScenarioInputs -> BalanceSheet -> Capital -> Scenario -> IncomeStatement -> CashFlowStatement
computeCashFlowStatement ScenarioInputs{..} balanceSheet0 Station{..} Scenario{..} incomeStatement@IncomeStatement{..} =
  let
    Expenses{..} = expenses
    Taxation{..} = taxation
    netIncome = total incomeStatement
    depreciation = head equipmentDepreciation
    netCash = depreciation + netIncome
    capitalExpenditures = - stationCapitalCost
    capitalExpendituresForEquipmentInstallation = - stationInstallationCost
    netCashProvidedByInvestingActivities = capitalExpenditures + capitalExpendituresForEquipmentInstallation
    --  FIXME: need to account for crowd-funding here
    netIssuanceOfEquity1 =
      if durationOfDebt == 0
        then - (netCashProvidedByInvestingActivities + receiptOfOneTimeCapitalIncentive) / (1 + minimumDebtToEquityRatio)
        else netCashUsedInFinancingActivities1 - netIssuanceOfDebt1 - receiptOfOneTimeCapitalIncentive
    netIssuanceOfEquity =
      if netIssuanceOfEquity1 > 0 && netIssuanceOfDebt1 == 0
        then netIssuanceOfEquity1 / (1 + minimumDebtToEquityRatio)
        else netIssuanceOfEquity1
    accumulatedDebt0 = Liabilities.accumulatedDebt $ BalanceSheet.liabilities balanceSheet0
    netIssuanceOfDebt1 =
      if durationOfDebt == 0
        then - netCashProvidedByInvestingActivities - netIssuanceOfEquity1 - receiptOfOneTimeCapitalIncentive
        else case debtType of
          OneTimeLoan   -> if durationOfDebt <= loanPeriod
                             then sum [ppmt (debtInterestRate / 12) per ((loanPeriod - durationOfDebt + 1) * 12) accumulatedDebt0 Nothing Nothing | per <- [1..12]]
                             else 0
          RevolvingDebt -> if durationOfDebt == ceiling (installationTime + equipmentLife)
                             then - accumulatedDebt0
                             else 0
    netIssuanceOfDebt =
      if netIssuanceOfEquity1 > 0 && netIssuanceOfDebt1 == 0
        then netIssuanceOfEquity * minimumDebtToEquityRatio
        else netIssuanceOfDebt1
    netCashUsedInFinancingActivities1 =
      if durationOfDebt == 0
        then netIssuanceOfDebt1 + netIssuanceOfEquity1 + receiptOfOneTimeCapitalIncentive
        else netChangeOfCashAndCashEquivalents1 - netCashProvidedByInvestingActivities - netCash
    netCashUsedInFinancingActivities =
      if durationOfDebt == 0
        then netIssuanceOfDebt + netIssuanceOfEquity + receiptOfOneTimeCapitalIncentive
        else netChangeOfCashAndCashEquivalents - netCashProvidedByInvestingActivities - netCash
    receiptOfOneTimeCapitalIncentive = newCapitalIncentives
    accumulatedCash0 = Assets.accumulatedCash $ BalanceSheet.assets balanceSheet0
    netChangeOfCashAndCashEquivalents1
      | durationOfDebt == 0                                         = netCash + netCashProvidedByInvestingActivities + netCashUsedInFinancingActivities1
      | durationOfDebt < ceiling (installationTime + equipmentLife) = (total operatingExpenses + interestOnOutstandingDebt + taxesDue) / 12 * cashOnHandRatio - accumulatedCash0
      | otherwise                                                   = - accumulatedCash0
    netChangeOfCashAndCashEquivalents
      | durationOfDebt == 0                                         = netCash + netCashProvidedByInvestingActivities + netCashUsedInFinancingActivities
      | durationOfDebt < ceiling (installationTime + equipmentLife) = (total operatingExpenses + interestOnOutstandingDebt + taxesDue) / 12 * cashOnHandRatio - accumulatedCash0
      | otherwise                                                   = - accumulatedCash0
  in
    CashFlowStatement{..}


computeBalanceSheet :: BalanceSheet -> Scenario -> CashFlowStatement -> Taxation -> BalanceSheet
computeBalanceSheet balanceSheet0 scenario cashFlowStatement taxation =
  let
    assets = computeAssets (BalanceSheet.assets balanceSheet0) cashFlowStatement taxation
    liabilities = computeLiabilities (BalanceSheet.liabilities balanceSheet0) cashFlowStatement
    equity = computeEquity (BalanceSheet.equity balanceSheet0) scenario cashFlowStatement taxation
  in
    BalanceSheet{..}


computeAssets :: Assets -> CashFlowStatement -> Taxation -> Assets
computeAssets cashFlowStatement0 CashFlowStatement{..} Taxation{..} =
  let
    accumulatedCash = Assets.accumulatedCash cashFlowStatement0 + netChangeOfCashAndCashEquivalents
    accumulatedPPE = Assets.accumulatedPPE cashFlowStatement0 - netCashProvidedByInvestingActivities
    accumulatedDepreciation = Assets.accumulatedDepreciation cashFlowStatement0 - depreciation
    netPPE = accumulatedPPE + accumulatedDepreciation
    accumulatedDeferredTaxLosses = - sum taxLossCarryForward
  in
    Assets{..}
   
 
computeLiabilities :: Liabilities -> CashFlowStatement -> Liabilities
computeLiabilities liabilities0 CashFlowStatement{..} =
  let
    accumulatedDebt = Liabilities.accumulatedDebt liabilities0 + netIssuanceOfDebt
  in
    Liabilities{..}


computeEquity :: Equity -> Scenario -> CashFlowStatement -> Taxation -> Equity
computeEquity equity0 Scenario{..} CashFlowStatement{..} Taxation{..} =
  let
    accumulatedEquityFromCapitalIncentives = Equity.accumulatedEquityFromCapitalIncentives equity0 + newCapitalIncentives
    accumulatedEquityFromInvestorContribution = Equity.accumulatedEquityFromInvestorContribution equity0 + maximum [netIssuanceOfEquity, 0]
    retainedEarnings = Equity.retainedEarnings equity0 + netIncome + minimum [netIssuanceOfEquity, 0]
    accumulatedDeferredTaxLosses' = - sum taxLossCarryForward
  in
    Equity{..}


computePerformanceAnalyses :: ScenarioInputs -> [(Scenario, Finances)] -> [PerformanceAnalysis]
computePerformanceAnalyses = (. (tail . inits)) . map . computePerformanceAnalysis


computePerformanceAnalysis :: ScenarioInputs -> [(Scenario, Finances)] -> PerformanceAnalysis
computePerformanceAnalysis ScenarioInputs{..} scenarioFinances =
  let

    (Scenario{..}, Finances{..}) = last scenarioFinances
    IncomeStatement{..} = incomeStatement
    Expenses{..} = expenses
    OperatingExpenses{..} = operatingExpenses
    Taxation{..} = taxation
    CashFlowStatement{..} = cashFlowStatement
    BalanceSheet{..} = balanceSheet
    Equity{..} = equity
    
    finances = map snd scenarioFinances
    scenarios = map fst scenarioFinances
    hydrogenSales' :: [Double]
    hydrogenSales' = map ((* days_per_year) . Scenario.hydrogenSales) scenarios

    ebitdbPositive = earningsBeforeInterestTaxesAndDepreciation > 0
    investorNetCashFlow' :: [Double]
    investorNetCashFlow' = map (negate . CashFlowStatement.netIssuanceOfEquity . Finances.cashFlowStatement) finances
    investorNetCashFlow = last investorNetCashFlow'
    investorCumulativeCashFlow = sum investorNetCashFlow'
    cumulativeCashFlowPositive = investorCumulativeCashFlow > 0
    internalRateOfReturn = irr investorNetCashFlow'
    netPresentValue = head investorNetCashFlow' + F.netPresentValue discountRate (tail investorNetCashFlow')

    totalRevenues' :: [Double]
    totalRevenues' = map Finances.totalRevenues finances
    totalRevenues =
      (head totalRevenues' + F.netPresentValue generalInflationRate (tail totalRevenues'))
        / sum hydrogenSales' * (1 + generalInflationRate)

    totalExpenses' :: [Double]
    totalExpenses' = map Finances.totalExpenses finances
    totalExpenses =
      (head totalExpenses' + F.netPresentValue generalInflationRate (tail totalExpenses'))
        / sum hydrogenSales' * (1 + generalInflationRate)

    breakEvenHydrogenPrice = hydrogenPrice / (1 + generalInflationRate)^^(durationOfDebt - 1)

    monetizedTaxLosses = - minimum [taxesDue, 0]

    grossMargin = (total revenues - costOfGoodsSold) / last hydrogenSales'
    grossMarginUnnormalized = 1 - costOfGoodsSold / total revenues

    costOfGoodsSold =
      total operatingExpenses
        - sellingAndAdministrative
        + interestOnOutstandingDebt
        + head equipmentDepreciation
    normalizedCostOfGoodsSold = costOfGoodsSold / last hydrogenSales'

    investorEquity = total equity
    investorEquityLessCapitalIncentive = investorEquity - accumulatedEquityFromCapitalIncentives
    returnOnInvestorEquity = netIncome / investorEquityLessCapitalIncentive
    returnOnTotalEquity = netIncome / investorEquity
    debtOverEquity = total liabilities / total equity
    debtCoverageRatio = earningsBeforeInterestTaxesAndDepreciation / interestOnOutstandingDebt
  in
    PerformanceAnalysis{..}
