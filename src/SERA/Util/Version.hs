module SERA.Util.Version {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  numericVersion
, stringVersion
) where


import Data.Version (Version(..), showVersion)
import Paths_sera (version)


numericVersion :: [Int]
numericVersion = versionBranch version


stringVersion :: String
stringVersion = showVersion version ++ " (2015)"
