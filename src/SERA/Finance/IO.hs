{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.IO {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  dumpFinances
, tsv2csv
) where


import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Table (tabulationsT')
import SERA.Configuration.RiskInputs (RiskInputs)
import SERA.Configuration.ScenarioInputs (ScenarioInputs)
import SERA.Finance.Analysis.Finances (Finances)
import SERA.Finance.Analysis.PerformanceAnalysis (PerformanceAnalysis)
import SERA.Finance.Capital (Capital)
import SERA.Finance.Project (Project(..))
import SERA.Finance.Scenario (Scenario)
import System.IO (IOMode(WriteMode), hClose, hPutStrLn, openFile)


dumpFinances :: FilePath -> (Project, Maybe RiskInputs, ScenarioInputs, [Capital], [Scenario], [Finances], [PerformanceAnalysis]) -> IO ()
dumpFinances directory (region@Project{..}, risks, parameters, stations, scenarios, finances, overall) =
  do
    h <- openFile (directory ++ "/finance-" ++ projectId ++ ".csv") WriteMode
    hPutStrLn h $ tsv2csv $ tabulationsT' [region]
    hPutStrLn h $ tsv2csv $ tabulationsT' [parameters]
    hPutStrLn h $ tsv2csv $ tabulationsT' stations
    hPutStrLn h $ tsv2csv $ tabulationsT' scenarios
    hPutStrLn h $ tsv2csv $ tabulationsT' finances
    hPutStrLn h $ tsv2csv $ tabulationsT' overall
    maybe (return ()) (hPutStrLn h . tsv2csv . tabulationsT' . return) risks
    hClose h


tsv2csv :: String -> String
tsv2csv =
  unlines
    . map (intercalate "," . map (('\"' :) . (++ "\"")) . splitOn "\t")
    . lines
