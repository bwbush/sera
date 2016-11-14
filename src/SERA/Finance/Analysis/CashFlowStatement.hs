{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.CashFlowStatement {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  CashFlowStatement(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Util.Summarization (Summable(..))


data CashFlowStatement =
  CashFlowStatement
    {
      netIncome                                   :: Double
    , depreciation                                :: Double
    , netCash                                     :: Double
    , capitalExpenditures                         :: Double
    , capitalExpendituresForEquipmentInstallation :: Double
    , netCashProvidedByInvestingActivities        :: Double
    , netIssuanceOfDebt                           :: Double
    , netIssuanceOfEquity                         :: Double
    , receiptOfOneTimeCapitalIncentive            :: Double
    , netCashUsedInFinancingActivities            :: Double
    , netChangeOfCashAndCashEquivalents           :: Double
    }
    deriving (Generic, Read, Show)

instance FromJSON CashFlowStatement

instance ToJSON CashFlowStatement where
  toJSON = genericToJSON defaultOptions

instance Zero CashFlowStatement where
  zero = CashFlowStatement 0 0 0 0 0 0 0 0 0 0 0

instance Summable CashFlowStatement where
  summation x =
    CashFlowStatement
    {
      netIncome                                   = sum' netIncome
    , depreciation                                = sum' depreciation
    , netCash                                     = sum' netCash
    , capitalExpenditures                         = sum' capitalExpenditures
    , capitalExpendituresForEquipmentInstallation = sum' capitalExpendituresForEquipmentInstallation
    , netCashProvidedByInvestingActivities        = sum' netCashProvidedByInvestingActivities
    , netIssuanceOfDebt                           = sum' netIssuanceOfDebt
    , netIssuanceOfEquity                         = sum' netIssuanceOfEquity
    , receiptOfOneTimeCapitalIncentive            = sum' receiptOfOneTimeCapitalIncentive
    , netCashUsedInFinancingActivities            = sum' netCashUsedInFinancingActivities
    , netChangeOfCashAndCashEquivalents           = sum' netChangeOfCashAndCashEquivalents
    }
      where sum' = sum . flip map x

instance Tabulatable CashFlowStatement where
  labels CashFlowStatement{..} =
    [
      "CASH FLOW STATEMENT"
    , "Net Income [$]"
    , "Adjustments to Reconcile Net Income to Net Cash"
    , "  Depreciation [$]"
    , "  Net Cash [$]"
    , "Cash Flows from Investing Activities"
    , "  Capital Expenditures for Equipment [$]"
    , "  Capital Expenditures for Equipment Installation [$]"
    , "  Net Cash Provided by Investing Activities [$]"
    , "Cash Flows from Financing Activities"
    , "  Net Issuance of Debt [$]"
    , "  Net Issuance of Equity [$]"
    , "  Receipt of One Time Capital Incentive [$]"
    , "  Net Cash Used in Financing Activities [$]"
    , "Net Change of Cash and Cash Equivalents [$]"
    ]
  tabulation CashFlowStatement{..} =
    [
      ""
    , show netIncome
    , ""
    , show depreciation
    , show netCash
    , ""
    , show capitalExpenditures
    , show capitalExpendituresForEquipmentInstallation
    , show netCashProvidedByInvestingActivities
    , ""
    , show netIssuanceOfDebt
    , show netIssuanceOfEquity
    , show receiptOfOneTimeCapitalIncentive
    , show netCashUsedInFinancingActivities
    , show netChangeOfCashAndCashEquivalents
    ]
  untabulation = undefined  -- TODO: to be implemented
