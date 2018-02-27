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


{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA (
-- * Versioning
  numericVersion
, stringVersion
-- * Logging
, SeraLog
, withSeraLog
, debug
, trace'
, inform
, unsafeInform
, unsafePrint
-- * I/O
, verboseReadFieldCubeSource
, verboseWriteFieldCubeSource

, checkPresent
, checkDisjoint
, checkDuplicates
) where


import Control.Monad (guard, unless, void)
import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import Control.Monad.Log (MonadLog, LoggingT, Severity(..), WithSeverity(..), renderWithSeverity, runLoggingT)
import Data.Daft.Source (DataSource(..), withSource)
import Data.Daft.TypeLevel (Union)
import Data.Daft.Vinyl.FieldCube (type (↝), ε)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeSource, writeFieldCubeSource)
import Data.Daft.Vinyl.FieldRec (Labeled)
import Data.Daft.Vinyl.FieldRec.IO (ReadFieldRec, ShowFieldRec)
import Data.Daft.Vinyl.FieldRec.Instances ()
import Data.Function.MapReduce (groupReduceByKey)
import Data.Maybe (catMaybes)
import Data.Set (Set, (\\))
import Data.String (IsString(..))
import Data.Version (Version(..), showVersion)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (⊆))
import Data.Vinyl.TypeLevel (type (++))
import Debug.Trace (trace)
import Paths_sera (version)
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Set as S (intersection, toList)

-- | Report the numeric version.
numericVersion :: [Int]
numericVersion = versionBranch version


-- | Report the version string.
stringVersion :: String
stringVersion = showVersion version ++ " (2017)"


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


unsafePrint :: String -> a -> a
unsafePrint s x =
  unsafePerformIO
    $ do
      putStrLn s
      return x


type SeraLog = MonadLog (WithSeverity String)


withSeraLog :: (IsString e, MonadError e m, MonadIO m) => LoggingT (WithSeverity String) m a -> m a
withSeraLog f =
  runLoggingT
    f
    (
      \message ->
        unless ( msgSeverity message >= Debug)
          $ if msgSeverity message > Critical
              then liftIO . hPrint stderr $ renderWithSeverity fromString message
              else throwError . fromString $ discardSeverity message
    )


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

checkPresent :: (Ord a, Show a, SeraLog m) => (String -> m ()) -> String -> Set a -> String -> Set a -> m ()
checkPresent logMessage label records label' records' =
  sequence_
    [
      logMessage $ label ++ " references \"" ++ show value ++ "\" not present in " ++ label' ++ "."
    |
      value <- S.toList $ records \\ records'
    ]


checkDisjoint :: (Ord a, Show a, SeraLog m) => (String -> m ()) -> String -> Set a -> Set a -> m ()
checkDisjoint logMessage message records records' =
  sequence_
    [
      logMessage $ message ++ " \"" ++ show value ++ "\"."
    |
      value <- S.toList $ records `S.intersection` records'
    ]


checkDuplicates :: (SeraLog m, Ord b, Show b) => (String -> m ()) -> String -> (a -> b) -> [a] -> m ()
checkDuplicates logMessage label field records =
  sequence_
    [
      logMessage $ "Duplicate " ++ label ++ " \"" ++ show location ++ "\"."
    |
      location <-
        catMaybes
          $ groupReduceByKey
            field
            (flip ((>>) . guard . (> 1) . length) . return)
            records
    ]
