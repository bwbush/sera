-----------------------------------------------------------------------------
--
-- Module      :  $Header$
-- Copyright   :  (c) 2018 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Processes.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Process (
-- * Types
  ProcessLibrary(..)
, Technologies
-- * FUnctions
, isProduction
, filterTechnologiesByProductive
, productions
, productions'
, deliveries
, processes
, pathways
, Pathways
, localPathways
, transmissionPathways
-- * Input/output
, ProcessLibraryFiles(..)
, readProcessLibrary
-- * Quality assurance
, checkProcessLibrary
) where


import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Log (logCritical, logError, logInfo, logWarning)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.Vinyl.FieldCube (type (*↝), κ', σ, υ, ω, toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import GHC.Generics (Generic)
import SERA (SeraLog, checkCondition, checkPresent, readConcat)
import SERA.Types.Cubes (ExistingCube, PathwayCube, ProcessCostCube, ProcessInputCube, ProcessOutputCube)
import SERA.Types.Fields (fNameplate, FCondition, fCondition, fDelivery, fExtended, Pathway, FPathway, fPathway, Productive(..), fProductive, FStage, fStage, Technology, FTechnology, fTechnology, fTransmission)
import SERA.Util (extractKey, extractValue)

import qualified Data.Set as S (map, union)


isProduction :: Productive -> Bool
isProduction No = False
isProduction _  = True


-- TO DO: eliminate distance and add linear distance scaling, also add capacity scaling exponent.


data ProcessLibrary =
  ProcessLibrary
  {
    processCostCube   :: ProcessCostCube
  , processInputCube  :: ProcessInputCube
  , processOutputCube :: ProcessOutputCube
  , pathwayCube       :: PathwayCube
  }
    deriving (Eq, Ord, Show)


type Technologies = Set (FieldRec '[FTechnology])


filterTechnologiesByProductive :: (Productive -> Bool) -> ProcessLibrary -> Set Technology
filterTechnologiesByProductive f ProcessLibrary{..} =
  S.map (fTechnology <:)
    $ (ω $ σ (const $ f . (fProductive <:)) processCostCube :: Technologies)


productions :: ProcessLibrary -> Set Technology
productions = filterTechnologiesByProductive isProduction


productions' :: Productive -> ProcessLibrary -> Set Technology
productions' = filterTechnologiesByProductive . (==)


deliveries :: ProcessLibrary -> Set Technology
deliveries = filterTechnologiesByProductive $ not . isProduction


processes :: ProcessLibrary -> Set Technology
processes = filterTechnologiesByProductive $ const True


pathways :: ProcessLibrary -> Set Pathway
pathways ProcessLibrary{..} = υ (fPathway <:) pathwayCube


type Pathways = Set (FieldRec '[FPathway])


localPathways :: ProcessLibrary -> Set Pathway
localPathways ProcessLibrary{..} =
  let
    results :: '[FPathway] *↝ '[FCondition]
    results = 
      σ (\_ rec -> fCondition <: rec)
        $ κ' (ω pathwayCube :: Set (FieldRec '[FStage]))
          (
            \recs -> 
              let
                recs' = sortBy (compare `on` (fStage <:)) recs
              in
                fCondition =: not ((fExtended <:) $ head recs') && all (fDelivery <:) (tail recs')
          )
          pathwayCube
  in
    S.map (fPathway <:)
      $ (ω results :: Pathways)


transmissionPathways :: ProcessLibrary -> Set Pathway
transmissionPathways ProcessLibrary{..} =
  let
    results :: '[FPathway] *↝ '[FCondition]
    results = 
      σ (\_ rec -> fCondition <: rec)
        $ κ' (ω pathwayCube :: Set (FieldRec '[FStage]))
          (
            \recs -> fCondition =: any (fTransmission <:) recs
          )
          pathwayCube
  in
    S.map (fPathway <:)
      $ (ω results :: Pathways)


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


checkProcessLibrary :: SeraLog m => ProcessLibrary -> ExistingCube -> m ()
checkProcessLibrary ProcessLibrary{..} existingCube =
  do
    logInfo "Checking process library . . ."
    let
      costTechnologies     = extractKey   (fTechnology <:) processCostCube
      inputTechnologies    = extractKey   (fTechnology <:) processInputCube
      outputTechnologies   = extractKey   (fTechnology <:) processOutputCube
      pathwayTechnologies  = extractValue (fTechnology <:) pathwayCube
      deliveryTechnologies = extractKey   (fTechnology <:) $ σ (const $ not . isProduction . (fProductive <:)) processCostCube
      existingTechnologies = extractValue (fTechnology <:) existingCube
    checkPresent
      logError
      "Process inputs"
      inputTechnologies
      "process costs or existing technologies"
      (costTechnologies `S.union` existingTechnologies)
    checkPresent
      logError
      "Process outputs"
      outputTechnologies
      "process costs or existing technologies"
      (costTechnologies `S.union` existingTechnologies)
    checkPresent
      logCritical
      "Pathways"
      pathwayTechnologies
      "non-production process costs"
      deliveryTechnologies
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
      "Non-production process costs"
      deliveryTechnologies
      "pathways"
      pathwayTechnologies
    checkCondition
      logCritical
      "Process costs"
      ((> 0) . (fNameplate <:))
      "non-posiitive nameplate capacity"
      $ toKnownRecords processCostCube
    checkCondition
      logWarning
      "Process inputs"
      ((>= 0) . (fNameplate <:))
      "negative nameplate capacity"
      $ toKnownRecords processInputCube
    checkCondition
      logWarning
      "Process outputs"
      ((>= 0) . (fNameplate <:))
      "negative nameplate capacity"
      $ toKnownRecords processOutputCube
    checkPresent
      logWarning
      "Existing technologies"
      existingTechnologies
      "process inputs"
      inputTechnologies
    checkPresent
      logWarning
      "Existing technologies"
      existingTechnologies
      "process outputs"
      outputTechnologies
