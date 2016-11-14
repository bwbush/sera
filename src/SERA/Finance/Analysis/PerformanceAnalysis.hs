{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.PerformanceAnalysis {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  PerformanceAnalysis(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)


data PerformanceAnalysis =
  PerformanceAnalysis
    {
      cumulativeCashFlowPositive         :: Bool
    , ebitdbPositive                     :: Bool
    , investorNetCashFlow                :: Double
    , investorCumulativeCashFlow         :: Double
    , internalRateOfReturn               :: Double
    , netPresentValue                    :: Double
    , totalRevenues                      :: Double
    , totalExpenses                      :: Double
    , grossMargin                        :: Double
    , breakEvenHydrogenPrice             :: Double
    , monetizedTaxLosses                 :: Double
    , grossMarginUnnormalized            :: Double
    , costOfGoodsSold                    :: Double
    , normalizedCostOfGoodsSold          :: Double
    , investorEquity                     :: Double
    , investorEquityLessCapitalIncentive :: Double
    , returnOnInvestorEquity             :: Double
    , returnOnTotalEquity                :: Double
    , debtOverEquity                     :: Double
    , debtCoverageRatio                  :: Double
    }
    deriving (Generic, Read, Show)

instance FromJSON PerformanceAnalysis

instance ToJSON PerformanceAnalysis where
  toJSON = genericToJSON defaultOptions

instance Zero PerformanceAnalysis
  where zero = PerformanceAnalysis False False 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

instance Tabulatable PerformanceAnalysis where
  labels _ =
    [
      "FINANCIAL PERFORMANCE ANALYSIS"
    , "Positive EBITD?"
    , "Positive Cumulative Cash Flow?"
    , "Investor Net Cash Flow [$ / year]"
    , "Investor Cumulative Cash Flow [$]"
    , "Internal Rate of Return [fraction / year]"
    , "Net Present Value [$]"
    , "Total Revenues [$ / kgH2]"
    , "Total Expenses [$ / kgH2]"
    , "Gross Margin [$ / kgH2]"
    , "Break-Even Hydrogen Price [$ / kgH2]"
    , "Monetized Tax Losses [$]"
    , "Gross Margin [1]"
    , "Cost of Goods Sold [$]"
    , "Cost of Goods Sold [$ / kgH2]"
    , "Investor Equity [$]"
    , "Investor Equity Less Capital Incentive [$]"
    , "Return on Investor Equity [fraction / year]"
    , "Return on Total Equity [fraction / year]"
    , "Debt/Equity [fraction]"
    , "Debt Service Coverage Ratio [fraction]"
    ]
  tabulation PerformanceAnalysis{..} =
    [
      ""
    , show ebitdbPositive
    , show cumulativeCashFlowPositive
    , show investorNetCashFlow               
    , show investorCumulativeCashFlow        
    , show internalRateOfReturn              
    , show netPresentValue                   
    , show totalRevenues                     
    , show totalExpenses                     
    , show grossMargin                       
    , show breakEvenHydrogenPrice
    , show monetizedTaxLosses                
    , show grossMarginUnnormalized
    , show costOfGoodsSold                   
    , show normalizedCostOfGoodsSold         
    , show investorEquity                    
    , show investorEquityLessCapitalIncentive
    , show returnOnInvestorEquity            
    , show returnOnTotalEquity               
    , show debtOverEquity                    
    , show debtCoverageRatio                 
    ]
  untabulation = undefined  -- TODO: to be implemented
