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


{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA (
-- * Versioning
  numericVersion
, stringVersion
-- * Logging
, debug
, trace'
, inform
, unsafeInform
-- * I/O
, verboseReadFieldCubeSource
, verboseWriteFieldCubeSource
) where


import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Daft.Source (DataSource(..), withSource)
import Data.Daft.TypeLevel (Union)
import Data.Daft.Vinyl.FieldCube (type (↝), ε)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeSource, writeFieldCubeSource)
import Data.Daft.Vinyl.FieldRec (Labeled)
import Data.Daft.Vinyl.FieldRec.IO (ReadFieldRec, ShowFieldRec)
import Data.Daft.Vinyl.FieldRec.Instances ()
import Data.String (IsString(..))
import Data.Version (Version(..), showVersion)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆))
import Data.Vinyl.TypeLevel (type (++))
import Debug.Trace (trace)
import Paths_sera (version)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)


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


unsafeInform :: String -> a -> a
unsafeInform s x =
  unsafePerformIO
    $ do
      hPutStrLn stderr s
      return x


verboseReadFieldCubeSource :: forall ks vs e m a . (Show a, ks ⊆ Union ks vs, vs ⊆ Union ks vs, Ord (FieldRec ks), IsString e, MonadError e m, MonadIO m, Labeled (FieldRec (Union ks vs)), ReadFieldRec (Union ks vs)) => String -> DataSource a -> m (ks ↝ vs)
verboseReadFieldCubeSource label source =
  do
    inform $ "Reading " ++ label ++ " from " ++ show source ++ " . . ."
    ε <$> readFieldCubeSource source


verboseWriteFieldCubeSource :: forall ks vs e m a . (Show a, Ord (FieldRec ks), Labeled (FieldRec (ks ++ vs)), ShowFieldRec (ks ++ vs), IsString e, MonadError e m, MonadIO m) => String -> DataSource a -> ks ↝ vs -> m ()
verboseWriteFieldCubeSource label source table =
    withSource source $ \source' -> do
      inform $ "Writing " ++ label ++ " to " ++ show source ++ " . . ."
      void $ writeFieldCubeSource source' table
