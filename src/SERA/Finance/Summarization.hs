{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Summarization {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Summary(..)
, makeSummary
) where


import Data.Table (Tabulatable(..))
import SERA.Configuration.RiskInputs (RiskInputs)
import SERA.Configuration.ScenarioInputs (ScenarioInputs)
import SERA.Finance.Analysis.Finances (Finances(..))
import SERA.Finance.Analysis.PerformanceAnalysis (PerformanceAnalysis)
import SERA.Finance.Capital (Capital)
import SERA.Finance.Project (Project(..))
import SERA.Finance.Scenario (Scenario)
import SERA.Util.Time (Year(..))

import qualified SERA.Finance.Analysis.PerformanceAnalysis as F (PerformanceAnalysis(..))


data Summary =
  Summary
    {
      project                    :: Project
    , year                       :: Year
    , cumulativeCashFlowPositive :: Bool
    , ebitdbPositive             :: Bool
    , internalRateOfReturn       :: Double
    , breakEvenHydrogenPrice     :: Double
    , returnOnInvestorEquity     :: Double
    , returnOnTotalEquity        :: Double
    }
      deriving (Read, Show)

instance Tabulatable Summary where
  labels _ =
    [
      "PROJECT SUMMARY"
    , "Project ID"
    , "Project Name"
    , "Year"
    , "Positive EBITD?"
    , "Positive Cumulative Cash Flow?"
    , "Internal Rate of Return [fraction / year]"
    , "Break-Even Hydrogen Price [$ / kgH2]"
    , "Return on Investor Equity [fraction / year]"
    , "Return on Total Equity [fraction / year]"
    ]
  tabulation Summary{..} =
    [
      ""
    , projectName project
    , projectId project
    , show $ unYear year
    , show ebitdbPositive
    , show cumulativeCashFlowPositive
    , show internalRateOfReturn              
    , show breakEvenHydrogenPrice
    , show returnOnInvestorEquity            
    , show returnOnTotalEquity               
    ]
  untabulation = undefined  -- TODO: to be implemented


makeSummary :: [(Project, Maybe RiskInputs, ScenarioInputs, [Capital], [Scenario], [Finances], [PerformanceAnalysis])] -> [Summary]
makeSummary = concatMap makeSummary'


makeSummary' :: (Project, Maybe RiskInputs, ScenarioInputs, [Capital], [Scenario], [Finances], [PerformanceAnalysis]) -> [Summary]
makeSummary' (region, _, _, _, _, finance, performance) =
  zipWith (makeSummary'' region) finance performance


makeSummary'' :: Project -> Finances -> PerformanceAnalysis -> Summary
makeSummary'' region Finances{..} F.PerformanceAnalysis{..} =
  let
    project = region
    year    = Year financesYear
  in
    Summary {..}
