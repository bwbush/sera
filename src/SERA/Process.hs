{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Process (
  ProcessSizer
, sizeComponent
) where


import Data.Daft.DataCube (evaluate, selectKnownMinimum)
import Data.Daft.Vinyl.FieldCube (κ, σ, τ)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (singleton)
import SERA.Process.Types -- FIXME
import SERA.Types (Year, fYear)


type ProcessSizer = Technology -> Year -> Double -> Double -> Maybe Component


sizeComponent :: ProcessLibrary -> ProcessSizer
sizeComponent ProcessLibrary{..} technology year capacity distance = 
  do -- FIXME: Add interpolation
    let
      candidate key _ =
           technology == fTechnology <: key -- Technologies must match,
        && year       <= fYear       <: key -- must be available in the given year,
        && capacity   >= fCapacity   <: key -- must be large enough, and
        && distance   >= fDistance   <: key -- must be long enough.
    (componentSpecification, componentProperties) <- selectKnownMinimum $ σ candidate processCube
    componentCosts <- processCostCube `evaluate` componentSpecification
    let
      extract  = κ (singleton componentSpecification) (const head) . σ (const . (componentSpecification ==) . τ)
      componentInputs = extract processInputCube
      componentOutputs = extract processOutputCube
    return Component{..}
