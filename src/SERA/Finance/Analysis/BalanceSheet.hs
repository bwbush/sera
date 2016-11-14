{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.BalanceSheet {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  BalanceSheet(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Finance.Analysis.Assets (Assets)
import SERA.Finance.Analysis.Equity (Equity)
import SERA.Finance.Analysis.Liabilities (Liabilities)
import SERA.Util.Summarization (Summable(..), Totalable(..))


data BalanceSheet =
  BalanceSheet
    {
      assets      :: Assets
    , liabilities :: Liabilities
    , equity      :: Equity
    }
    deriving (Generic, Read, Show)

instance FromJSON BalanceSheet

instance ToJSON BalanceSheet where
  toJSON = genericToJSON defaultOptions

instance Totalable BalanceSheet where
  total BalanceSheet{..} = total assets - total liabilities - total equity

instance Summable BalanceSheet where
  summation x =
    BalanceSheet
    {
      assets      = summation' assets
    , liabilities = summation' liabilities
    , equity      = summation' equity
    }
      where
        summation' :: Summable a => (BalanceSheet -> a) -> a
        summation' = summation . flip map x

instance Zero BalanceSheet where
  zero = BalanceSheet zero zero zero

instance Tabulatable BalanceSheet where
  labels BalanceSheet{..} =
    [
      "BALANCE SHEET"
    ]
    ++
    labels assets
    ++
    labels liabilities
    ++
    labels equity
    ++
    [
      "Assets - Liabilities - Equity [$]"
    ]
  tabulation balanceSheet@BalanceSheet{..} =
    [
      ""
    ]
    ++
    tabulation assets
    ++
    tabulation liabilities
    ++
    tabulation equity
    ++
    [
      show $ total balanceSheet
    ]
  untabulation = undefined  -- TODO: to be implemented
