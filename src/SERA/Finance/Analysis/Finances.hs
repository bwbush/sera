{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.Finances {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Finances(..)
, totalRevenues
, totalExpenses
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Finance.Analysis.BalanceSheet (BalanceSheet)
import SERA.Finance.Analysis.CashFlowStatement (CashFlowStatement(..))
import SERA.Finance.Analysis.Expenses (Expenses(..))
import SERA.Finance.Analysis.IncomeStatement (IncomeStatement(..))
import SERA.Finance.Analysis.Revenues (Revenues(..))
import SERA.Finance.Analysis.Taxation (Taxation(..))
import SERA.Util.Summarization (Summable(..), Totalable(..))


data Finances =
  Finances
    {
      financesYear        :: Int
    , incomeStatement     :: IncomeStatement
    , cashFlowStatement   :: CashFlowStatement
    , balanceSheet        :: BalanceSheet
    }
    deriving (Generic, Read, Show)

instance FromJSON Finances

instance ToJSON Finances where
  toJSON = genericToJSON defaultOptions

instance Summable Finances where
  summation x =
    Finances
    {
      financesYear      = head      $ map financesYear      x
    , incomeStatement   = summation $ map incomeStatement   x
    , cashFlowStatement = summation $ map cashFlowStatement x
    , balanceSheet      = summation $ map balanceSheet      x
    }

instance Zero Finances where
  zero = Finances 0 zero zero zero

instance Tabulatable Finances where
  labels Finances{..} =
    ["Year", ""]
    ++ 
    labels incomeStatement
    ++ [""] ++
    labels cashFlowStatement
    ++ [""] ++
    labels balanceSheet
  tabulation Finances{..} =
    [show financesYear, ""]
    ++ tabulation incomeStatement
    ++ [""] ++
    tabulation cashFlowStatement
    ++ [""] ++
    tabulation balanceSheet
  untabulation = undefined  -- TODO: to be implemented


totalRevenues :: Finances -> Double
totalRevenues finances =
  let
    Finances{..} = finances
    IncomeStatement{..} = incomeStatement
    Revenues{..} = revenues
    CashFlowStatement{..} = cashFlowStatement
  in
    sales + productionIncentive + incidentalRevenue + receiptOfOneTimeCapitalIncentive


totalExpenses :: Finances -> Double
totalExpenses finances =
  let
    Finances{..} = finances
    IncomeStatement{..} = incomeStatement
    Revenues{..} = revenues
    Expenses{..} = expenses
    Taxation{..} = taxation
    CashFlowStatement{..} = cashFlowStatement
  in 
    - creditCardFees - salesTaxes - roadTaxes
      + total operatingExpenses
      + interestOnOutstandingDebt + taxesDue
      - capitalExpenditures - capitalExpendituresForEquipmentInstallation
