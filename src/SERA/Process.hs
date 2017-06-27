{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Process (
  productions
, deliveries
, processes
, pathways
, ProcessSizer
, PathwaySizer
, sizeComponent
, sizePathway
) where


import Control.Monad.Except (MonadError, MonadIO)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.Daft.DataCube (evaluate, selectKnownMaximum)
import Data.Daft.Vinyl.FieldCube (κ, σ, τ, ω)
import Data.Daft.Vinyl.FieldCube.IO (readFieldCubeFile)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Maybe (fromMaybe)
import Data.Set (Set, (\\), singleton, toList)
import Data.String (IsString)
import Data.Vinyl.Derived (FieldRec)
import GHC.Generics (Generic)
import SERA.Process.Types -- FIXME
import SERA.Types (Year, fYear)


productions :: ProcessLibrary -> [Technology]
productions ProcessLibrary{..} =
  fmap (fTechnology <:)
    . toList
    $ (ω $ σ (const $ isProduction . (fProduction <:)) processCostCube :: Set (FieldRec '[FTechnology]))


deliveries :: ProcessLibrary -> [Technology]
deliveries ProcessLibrary{..} =
  fmap (fTechnology <:)
    . toList
    $ (ω $ σ (const $ not . isProduction . (fProduction <:)) processCostCube :: Set (FieldRec '[FTechnology]))


processes :: ProcessLibrary -> [Technology]
processes ProcessLibrary{..} =
  fmap (fTechnology <:)
    . toList
    $ (ω processCostCube :: Set (FieldRec '[FTechnology]))


pathways :: ProcessLibrary -> [Pathway]
pathways ProcessLibrary{..} =
  fmap (fPathway <:)
    . toList
    $ (ω pathwayCube :: Set (FieldRec '[FPathway]))


type ProcessSizer = Technology -> Year -> Double -> Double -> Maybe Component


sizeComponent :: ProcessLibrary -> ProcessSizer
sizeComponent ProcessLibrary{..} process year capacity distance = 
  do -- FIXME: Add interpolation
    let
      candidate key _ =
           process  == fTechnology <: key -- Technologies must match exactly,
        && year     <= fYear       <: key -- must be available in the given year, and
        && capacity <= fCapacity   <: key -- must be large enough.
    (specification, costs) <- selectKnownMaximum $ σ candidate processCostCube
    let
      component = Left process
      scaleCost cost stretch =
        (cost <: costs + distance * stretch <: costs)
          * (capacity / fCapacity <: specification) ** (fScaling <: costs)
      extract = κ (singleton specification) (const head) . σ (const . (specification ==) . τ)
      production = fProduction <: costs
      lifetime = fLifetime <: costs
      capitalCost = scaleCost fCapitalCost fCapitalCostStretch 
      fixedCost = scaleCost fFixedCost fFixedCostStretch
      variableCost = scaleCost fVariableCost fVariableCostStretch
      inputs = extract processInputCube
      outputs = extract processOutputCube
    return Component{..}


type PathwaySizer = Pathway -> Year -> Double -> Double -> Double -> Maybe Component


sizePathway :: ProcessLibrary -> PathwaySizer
sizePathway ProcessLibrary{..} pathway year capacity transmissionDistance deliveryDistance =
  do
    let
      candidate key _ = pathway == fPathway <: key
      processes' = σ candidate pathwayCube
    undefined
