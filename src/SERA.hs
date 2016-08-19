module SERA (
  numericVersion
, stringVersion
, debug
, trace'
) where


import Data.Version (Version(..), showVersion)
import Debug.Trace (trace)
import Paths_sera (version)


numericVersion :: [Int]
numericVersion = versionBranch version


stringVersion :: String
stringVersion = showVersion version ++ " (2016)"


debug :: Bool
debug = True


trace' :: String -> b -> b
trace' =
  if debug
    then trace
    else const id
