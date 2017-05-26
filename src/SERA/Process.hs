{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Process (
  ProcessLibraryFiles(..)
, readProcessLibrary
, ProcessSizer
, sizeComponent
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (evaluate, selectKnownMaximum)
import Data.Daft.Vinyl.FieldCube (κ, σ, τ)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (singleton)
import Data.String (IsString)
import GHC.Generics (Generic)
import SERA.Process.Types -- FIXME
import SERA.Types (Year, fYear)


type ProcessSizer = Technology -> Year -> Double -> Double -> Maybe Component


data ProcessLibraryFiles =
  ProcessLibraryFiles
  {
    propertiesFile :: FilePath
  , costsFile      :: FilePath
  , inputsFile     :: FilePath
  , outputsFile    :: FilePath
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ProcessLibraryFiles

instance ToJSON ProcessLibraryFiles


readProcessLibrary :: (IsString e, MonadError e m, MonadIO m) => [ProcessLibraryFiles] -> m ProcessLibrary
readProcessLibrary processLibraryFiles =
  do 
    processCube       <- mconcat <$> mapM (readFieldCubeFile . propertiesFile) processLibraryFiles
    processCostCube   <- mconcat <$> mapM (readFieldCubeFile . costsFile     ) processLibraryFiles
    processInputCube  <- mconcat <$> mapM (readFieldCubeFile . inputsFile    ) processLibraryFiles
    processOutputCube <- mconcat <$> mapM (readFieldCubeFile . outputsFile   ) processLibraryFiles
    return ProcessLibrary{..}


sizeComponent :: ProcessLibrary -> ProcessSizer
sizeComponent ProcessLibrary{..} component year capacity distance = 
  do -- FIXME: Add interpolation
    let
      candidate key _ =
           component == fTechnology <: key -- Technologies must match exactly,
        && year      <= fYear       <: key -- must be available in the given year, and
        && capacity  <= fCapacity   <: key -- must be large enough.
    (specification, properties) <- selectKnownMaximum $ σ candidate processCube
    costs <- processCostCube `evaluate` specification
    let
      scaleCost cost stretch =
        (cost <: costs + distance * stretch <: costs)
          * (capacity / fCapacity <: specification) ** (fScaling <: costs)
      extract = κ (singleton specification) (const head) . σ (const . (specification ==) . τ)
      onSite = fOnSite <: properties
      lifetime = fLifetime <: properties
      yield = fYield <: costs
      capitalCost = scaleCost fCapitalCost fCapitalCostStretch 
      fixedCost = scaleCost fFixedCost fFixedCostStretch
      variableCost = scaleCost fVariableCost fVariableCostStretch
      inputs = extract processInputCube
      outputs = extract processOutputCube
    return Component{..}
