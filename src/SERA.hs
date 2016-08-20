module SERA (
  numericVersion
, stringVersion
, debug
, trace'
, inform
) where


import Control.Monad.Except (MonadIO, liftIO)
import Data.Version (Version(..), showVersion)
import Debug.Trace (trace)
import Paths_sera (version)
import System.IO (hPutStrLn, stderr)


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


inform :: MonadIO m => String -> m ()
inform = liftIO . hPutStrLn stderr
