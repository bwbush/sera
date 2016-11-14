{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Configuration.ScenarioInputs {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  ScenarioInputs(..)
, DebtType(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)


data ScenarioInputs =
  ScenarioInputs
    {
      installationTime                   :: Double
    , demandRampUpTime                   :: Double    --  year
    , longTermNominalUtilization         :: Double    --  1
    , equipmentLife                      :: Double    --  year
    , totalTaxRate                       :: Double    --  1
    , installationCostsDepreciable       :: Bool      --
    , operatingIncentivesTaxable         :: Bool      --
    , capitalIncentiveDepreciable        :: Bool      --
    , taxLossesMonetized                 :: Bool      --
    , allowableTaxLossCarryForward       :: Int       --  year
    , generalInflationRate               :: Double    --  1
    , depreciationPeriod                 :: Int       --  year
    , discountRate                       :: Double    --  1
    , minimumDebtToEquityRatio           :: Double    --  1
    , debtType                           :: DebtType  --  RevolvingDebt, OneTimeLoan
    , loanPeriod                         :: Int       --  year
    , debtInterestRate                   :: Double    --  1
    , cashOnHandRatio                    :: Double    --  1
    , capitalIncentive                   :: Double    --  $ / station
    , durationOfCapitalIncentive         :: Int       --  year
    , productionIncentiveStart           :: Double    --  $ / station
    , productionIncentiveAnnualDecrement :: Double    --  $ / station / year
    , durationOfProductionIncentive      :: Int       --  year
    , annualFractionOfFuelPrepaid        :: Double    --  1
    , multiYearFractionOfFuelPrepaid     :: Double    --  1
    , lengthOfMultiYearPrepayment        :: Int       --  year
    , durationOfFuelPrepayments          :: Int       --  year
    , oneTimeConsumerPayment             :: Double    --  $ / vehicle
    , consumerDiscountOnHydrogen         :: Double    --  1
    , limitOnConsumerPayments            :: Double    --  vehicle / station
    }
    deriving (Generic, Read, Show)

instance FromJSON ScenarioInputs

instance ToJSON ScenarioInputs where
  toJSON = genericToJSON defaultOptions


instance Tabulatable ScenarioInputs where
  labels _ =
    [
      "SCENARIO INPUTS"
    , "Installation Time [year]"
    , "Demand Ramp-Up time [year]"
    , "Long Term Nominal Utilization [fraction]"
    , "Equipment Life [year]"
    , "Total Tax Rate [fraction]"
    , "Installation Costs Depreciable?"
    , "Operating Incentives Taxable?"
    , "Capital Incentives Depreciable?"
    , "Tax Losses Monetized?"
    , "Allowable Tax Loss Carry-Forward [year]"
    , "General Inflation Rate [fraction / year]"
    , "Depreciation Period [year]"
    , "Discount Rate [fraction / year]"
    , "Minimum Debt to Equity Ratio [fraction]"
    , "Debt Type"
    , "Loan Period [year]"
    , "Debt Interest Rate [fraction / year]"
    , "Cash on Hand Ratio [fraction]"
    , "Capital Incentive [$ / station]"
    , "Duration of Capital Incentive [year]"
    , "Initial Production Incentive [$ / station]"
    , "Annual Decrement of Production Incentive [$ / station / year]"
    , "Duration of Production Incentive [year]"
    , "Annual Fraction of Fuel Prepaid [fraction]"
    , "Multi-Year Fraction of Fuel Prepaid [fraction]"
    , "Length of Multi-Year Prepayment [year]"
    , "Duration of Multi-Year Prepayment [year]"
    , "One-Time Consumer Payment [$ / station / vehicle]"
    , "Consumer Discount on Hydrogen [fraction]"
    , "Limit on Consumer Payments [vehicle / station]"
    ]
  tabulation ScenarioInputs{..} =
    [
      ""
    , show installationTime
    , show demandRampUpTime
    , show longTermNominalUtilization
    , show equipmentLife
    , show totalTaxRate
    , show installationCostsDepreciable
    , show operatingIncentivesTaxable
    , show capitalIncentiveDepreciable
    , show taxLossesMonetized
    , show allowableTaxLossCarryForward
    , show generalInflationRate
    , show depreciationPeriod
    , show discountRate
    , show minimumDebtToEquityRatio
    , showDebt debtType
    , show loanPeriod
    , show debtInterestRate
    , show cashOnHandRatio
    , show capitalIncentive
    , show durationOfCapitalIncentive
    , show productionIncentiveStart
    , show productionIncentiveAnnualDecrement
    , show durationOfProductionIncentive
    , show annualFractionOfFuelPrepaid
    , show multiYearFractionOfFuelPrepaid
    , show lengthOfMultiYearPrepayment
    , show durationOfFuelPrepayments
    , show oneTimeConsumerPayment
    , show consumerDiscountOnHydrogen
    , show limitOnConsumerPayments
    ]
  untabulation = undefined -- TODO: make this readable


data DebtType = RevolvingDebt | OneTimeLoan
  deriving (Generic, Read, Show)

instance FromJSON DebtType

instance ToJSON DebtType where
  toJSON = genericToJSON defaultOptions


showDebt :: DebtType -> String
showDebt RevolvingDebt = "Revolving Debt"
showDebt OneTimeLoan   = "One-Time Loan"
