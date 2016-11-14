module SERA.Finance.IO.Xlsx {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Results
, formatResultsAsFile
, formatResults
) where


import Codec.Archive.Zip (addEntryToArchive, findEntryByPath, fromArchive, fromEntry, toArchive, toEntry)
import Data.Maybe (fromJust, isJust)
import SERA.Finance.IO.Xlsx.Template (xlsxTemplate)
import Text.Regex.Posix

import qualified Data.ByteString.Lazy.Char8 as BS (ByteString, fromStrict, pack, unpack, writeFile)
import qualified Data.Map.Strict as M (Map, fromList, lookup)


type Results = [[String]]


formatResultsAsFile :: FilePath -> Results -> IO ()
formatResultsAsFile output = BS.writeFile output . formatResults


entryPath :: FilePath
entryPath = "xl/worksheets/sheet1.xml"


formatResults :: Results -> BS.ByteString
formatResults results =
  let
    archive = toArchive $ BS.fromStrict xlsxTemplate
    entry :: [String]
    Just entry =
      lines
        . BS.unpack
        . fromEntry
        <$> findEntryByPath entryPath archive
    results' = makeResultsMap results
    entry' = updateResults results' entry
    archive' =
      flip addEntryToArchive archive
        $ toEntry entryPath 0
        $ BS.pack
        $ unlines entry'
  in
    fromArchive archive'


booleanConstants :: [(String, Int)]
booleanConstants =
  [
    ("False", 0)
  , ("True" , 1)
  ]


stringConstants :: [(String, Int)]
stringConstants =
  [
    ("NaN"           , 164)
  , ("Infinity"      , 165)
  , ("-Infinity"     , 166)
  , ("One-Time Loan" , 167)
  , ("Revolving Debt",  16)
  ]


data CellType =
    CellNumber  String
  | CellBoolean String
  | CellString  String
  | CellUnknown


updateResults :: Results' -> [String] -> [String]
updateResults _ [] = []
updateResults _ xs@[_] = xs
updateResults results (x : xs@(_ : ys)) =
  let
    number :: [[String]]
    number = x =~ "^ *<c r=\"([A-Z]+[0-9]+)\" s=\".\"[^/]?>"
    number' :: [[String]]
    number' = x =~ "^ *<c r=\"([A-Z]+[0-9]+)\">"
    boolean :: [[String]]
    boolean = x =~ "^ *<c r=\"([A-Z]+[0-9]+)\" t=\"b\"[^/]?>"
    string :: [[String]]
    string = x =~ "^ *<c r=\"([A-Z]+[0-9]+)\" t=\"s\"[^/]?>"
    sourceType, targetType :: CellType
    cell :: String
    (targetType, cell)
      | not (null number ) = (CellNumber  undefined, head number  !! 1)
      | not (null number') = (CellNumber  undefined, head number' !! 1)
      | not (null boolean) = (CellBoolean undefined, head boolean !! 1)
      | not (null string ) = (CellString  undefined, head string  !! 1)
      | otherwise          = (CellUnknown, ""               )
    value :: Maybe String
    value = cell `M.lookup` results
    sourceType
      | value `elem` map (Just . fst) booleanConstants = CellBoolean $ show $ fromJust $ fromJust value `lookup` booleanConstants
      | value `elem` map (Just . fst) stringConstants  = CellString  $ show $ fromJust $ fromJust value `lookup` stringConstants
      | isJust value                                    = CellNumber  $ fromJust value
      | otherwise                                       = CellUnknown
    wrap :: String -> String
    wrap = ("<v>" ++) . (++ "</v>")
    wrap' :: String -> String
    wrap' = (("<c r=\"" ++ cell ++ "\" t=\"") ++) . (++ "\">")
  in
    case (sourceType, targetType) of
      (CellString  value', CellString  _) -> x         : wrap value' : updateResults results ys
      (CellBoolean value', CellBoolean _) -> x         : wrap value' : updateResults results ys
      (CellNumber  value', CellNumber  _) -> x         : wrap value' : updateResults results ys
      (CellString  value', CellNumber  _) -> wrap' "s" : wrap value' : updateResults results ys
      (CellBoolean value', CellNumber  _) -> wrap' "b" : wrap value' : updateResults results ys
      (CellUnknown       , CellUnknown  ) -> x                       : updateResults results xs
      (CellUnknown       , _            ) -> x         : "<v/>"      : updateResults results ys
      _                                   -> x                       : updateResults results xs


type Results' = M.Map String String


rows :: [String]
rows = map show ([1..] :: [Int])


cols :: [String]
cols = map (: []) ['A'..'Z'] ++ [p : c | p <- ['A'..'Z'], c <- cols]


makeResultsMap :: Results -> Results'
makeResultsMap =
  M.fromList
    . concat
    . zipWith (\i -> zipWith (\j c -> (j ++ i, c)) cols) rows
