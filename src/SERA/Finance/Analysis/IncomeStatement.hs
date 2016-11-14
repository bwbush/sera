{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.IncomeStatement {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  IncomeStatement(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Finance.Analysis.Expenses (Expenses(..))
import SERA.Finance.Analysis.Revenues (Revenues)
import SERA.Finance.Analysis.Taxation (Taxation(..))
import SERA.Util.Summarization (Summable(..), Totalable(..))


data IncomeStatement =
  IncomeStatement {
    revenues :: Revenues
  , expenses :: Expenses
  }
    deriving (Generic, Read, Show)

instance FromJSON IncomeStatement

instance ToJSON IncomeStatement where
  toJSON = genericToJSON defaultOptions

instance Totalable IncomeStatement where
  total IncomeStatement{..} =
    let
      Expenses{..} = expenses
      Taxation{..} = taxation
    in
      total revenues - total operatingExpenses - interestOnOutstandingDebt - head equipmentDepreciation - taxesDue

instance Summable IncomeStatement where
  summation x =
    IncomeStatement
    {
      revenues = summation' revenues
    , expenses = summation' expenses
    }
      where
        summation' :: Summable a => (IncomeStatement -> a) -> a
        summation' = summation . flip map x

instance Zero IncomeStatement where
  zero = IncomeStatement zero zero 

instance Tabulatable IncomeStatement where
  labels IncomeStatement{..} =
    [
      "INCOME STATEMENT"
    ]
    ++
    labels revenues
    ++
    labels expenses
    ++
    [
      "Net Income [$]"
    ]
  tabulation incomeStatement@IncomeStatement{..} =
    [
      ""
    ]
    ++
    tabulation revenues
    ++
    tabulation expenses
    ++
    [
      show $ total incomeStatement
    ]
  untabulation = undefined  -- TODO: to be implemented
