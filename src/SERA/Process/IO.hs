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
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA (SeraLog, readConcat)
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


readProcessLibrary :: (IsString e, MonadError e m, MonadIO m, SeraLog m) => [ProcessLibraryFiles] -> [FilePath] -> m ProcessLibrary
readProcessLibrary processLibraryFiles pathwayFiles =
  do 
    processCostCube   <- readConcat "process costs"   "process cost key"   $                costsFile   <$> processLibraryFiles
    processInputCube  <- readConcat "process inputs"  "process input key"  $ fromMaybe [] . inputsFile  <$> processLibraryFiles
    processOutputCube <- readConcat "process outputs" "process output key" $ fromMaybe [] . outputsFile <$> processLibraryFiles
    pathwayCube       <- readConcat "pathways"        "pathwya key"                                         pathwayFiles
    return ProcessLibrary{..}
