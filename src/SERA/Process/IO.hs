{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process.IO (
  ProcessLibraryFiles(..)
, readProcessLibrary
, checkProcessLibrary
) where


import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logError, logInfo, logWarning)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.Vinyl.FieldCube (σ)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA (SeraLog, checkPresent, readConcat)
import SERA.Process.Types -- FIXME
import SERA.Util (extractKey, extractValue)


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
    pathwayCube       <- readConcat "pathways"        "pathway key"                                         pathwayFiles
    return ProcessLibrary{..}


checkProcessLibrary :: SeraLog m => ProcessLibrary -> m ()
checkProcessLibrary ProcessLibrary{..} =
  do
    logInfo "Checking process library . . ."
    let
      costTechnologies     = extractKey   (fTechnology <:) processCostCube
      inputTechnologies    = extractKey   (fTechnology <:) processInputCube
      outputTechnologies   = extractKey   (fTechnology <:) processOutputCube
      pathwayTechnologies  = extractValue (fTechnology <:) pathwayCube
      deliveryTechnologies = extractKey   (fTechnology <:) $ σ (const $ not . isProduction . (fProductive <:)) processCostCube
    checkPresent
      logError
      "Process inputs"
      inputTechnologies
      "process costs"
      costTechnologies
    checkPresent
      logError
      "Process outputs"
      outputTechnologies
      "process costs"
      costTechnologies
    checkPresent
      logError
      "Pathways"
      pathwayTechnologies
      "process costs"
      costTechnologies
    checkPresent
      logWarning
      "Process costs"
      costTechnologies
      "process inputs"
      inputTechnologies
    checkPresent
      logWarning
      "Process costs"
      costTechnologies
      "process outputs"
      outputTechnologies
    checkPresent
      logWarning
      "Process costs"
      deliveryTechnologies
      "pathways"
      pathwayTechnologies
