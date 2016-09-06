-----------------------------------------------------------------------------
--
-- Module      :  SERA.Service
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Services for SERA.
--
-----------------------------------------------------------------------------


module SERA (
-- * Versioning
  numericVersion
, stringVersion
-- * Logging
, debug
, trace'
, inform
) where


import Control.Monad.Except (MonadIO, liftIO)
import Data.Version (Version(..), showVersion)
import Debug.Trace (trace)
import Paths_sera (version)
import System.IO (hPutStrLn, stderr)


-- | Report the numeric version.
numericVersion :: [Int]
numericVersion = versionBranch version


-- | Report the version string.
stringVersion :: String
stringVersion = showVersion version ++ " (2016)"


-- | Whether debugging is activated.
debug :: Bool
debug = True


-- | Write to standard error during a computataon.
trace' :: String -> b -> b
trace' =
  if debug
    then trace
    else const id


-- | Write to standard error.
inform :: MonadIO m => String -> m ()
inform = liftIO . hPutStrLn stderr
