{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.Equity {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Equity(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Util.Summarization (Summable(..), Totalable(..))


data Equity =
  Equity
    {
      accumulatedEquityFromCapitalIncentives         :: Double
    , accumulatedEquityFromInvestorContribution      :: Double
    , retainedEarnings                               :: Double
    , accumulatedDeferredTaxLosses'                  :: Double
    }
    deriving (Generic, Read, Show)

instance FromJSON Equity

instance ToJSON Equity where
  toJSON = genericToJSON defaultOptions

instance Totalable Equity where
  total Equity{..} =
    accumulatedEquityFromCapitalIncentives
    + accumulatedEquityFromInvestorContribution
    + retainedEarnings
    + accumulatedDeferredTaxLosses'

instance Summable Equity where
  summation x =
    Equity
    {
      accumulatedEquityFromCapitalIncentives         = sum' accumulatedEquityFromCapitalIncentives
    , accumulatedEquityFromInvestorContribution      = sum' accumulatedEquityFromInvestorContribution
    , retainedEarnings                               = sum' retainedEarnings
    , accumulatedDeferredTaxLosses'                  = sum' accumulatedDeferredTaxLosses'
    }
      where sum' = sum . flip map x

instance Zero Equity where
  zero = Equity 0 0 0 0

instance Tabulatable Equity where
  labels _ =
    [
      "Equity"
    , "    Cumulative Equity from Capital Incentives [$]"
    , "    Cumulative Equity (Investor Contribution) [$]"
    , "    Retained Earnings [$]"
    , "    Cumulative Deferred Tax Losses [$]"
    , "  Total Equity [$]"
    ]
  tabulation equity@Equity{..} =
    [
      ""
    , show accumulatedEquityFromCapitalIncentives
    , show accumulatedEquityFromInvestorContribution
    , show retainedEarnings
    , show accumulatedDeferredTaxLosses'
    , show $ total equity
    ]
  untabulation = undefined  -- TODO: to be implemented
