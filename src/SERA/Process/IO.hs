{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process.IO (
  ProcessLibraryFiles(..)
, readProcessLibrary
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA.Process.Types -- FIXME


data ProcessLibraryFiles =
  ProcessLibraryFiles
  {
    costsFile   :: FilePath
  , inputsFile  :: Maybe FilePath
  , outputsFile :: Maybe FilePath

  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ProcessLibraryFiles

instance ToJSON ProcessLibraryFiles


readProcessLibrary :: (IsString e, MonadError e m, MonadIO m) => [ProcessLibraryFiles] -> [FilePath] -> m ProcessLibrary
readProcessLibrary processLibraryFiles pathwayFiles =
  do 
    processCostCube   <- mconcat <$> mapM (                      readFieldCubeFile . costsFile  ) processLibraryFiles
    processInputCube  <- mconcat <$> mapM (maybe (return mempty) readFieldCubeFile . inputsFile ) processLibraryFiles
    processOutputCube <- mconcat <$> mapM (maybe (return mempty) readFieldCubeFile . outputsFile) processLibraryFiles
    pathwayCube       <- mconcat <$> mapM                        readFieldCubeFile                pathwayFiles
    return ProcessLibrary{..}
