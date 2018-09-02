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
-- | Sizing and reification of process technologies.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}


module SERA.Process.Reification (
-- * Types
  TechnologyReifier
, TechnologyOperation
-- * Sizing
, sizeTechnology
, sizeInputs
, sizeOutputs
, sizeIntensity
-- * Reification
, technologyReifier
, operationReifier
) where


import Data.Daft.DataCube (knownKeys)
import Data.Daft.Vinyl.FieldCube ((!), σ, τ, toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Function (on)
import Data.Function.MapReduce (mapReduce)
import Data.List (nub, sortBy)
import Data.Set (Set)
import Data.Vinyl.Derived (FieldRec)
import Data.Vinyl.Lens (type (∈))
import SERA (trace')
import SERA.Material (Pricer)
import SERA.Process (ProcessLibrary(..), isProduction)
import SERA.Types.Cubes (IntensityCube)
import SERA.Types.Fields (fCapitalCost, fCapitalCostStretch, CostCategory(..), fConsumptionRate, fConsumptionRateStretch, fCostCategory, FDutyCycle, fDutyCycle, fFixedCost, fFixedCostStretch, fFlow, ImpactCategory(..), fImpactCategory, FInfrastructure, fInfrastructure, FIntensity, fIntensity, fLength, fLifetime, FLocation, fLoss, Material, FMaterial, fMaterial, FNameplate, fNameplate, fProduction, fProductionRate, fProductionRateStretch, Productive(..), fProductive, fQuantity, fSale, fSalvage, fScaling, fStorage, Technology, FTechnology, fTechnology, FUpstreamMaterial, fUpstreamMaterial, fVariableCost, fVariableCostStretch, Year, FYear, fYear)
import SERA.Types.Records (Cash, Construction, Flow, Impact, ProcessCost)

import qualified Data.Set as S (toList)


-- | Operate a technology for a year at a given flow rate.
type TechnologyOperation =  Pricer
                         -> Year                             -- ^ The year.
                         -> Double                           -- ^ The flow rate [kg/yr].
                         -> (Flow, [Cash], [Impact], Double) -- ^ The flow, cash, and impact records, along with the flow.


-- | Reify a technology.
type TechnologyReifier =  FieldRec '[FInfrastructure, FLocation]    -- ^ The infrastructure ID and location.
                       -> Year                                      -- ^ The year.
                       -> Double                                    -- ^ The flow rate [kg/yr].
                       -> Double                                    -- ^ The length [km].
                       -> Technology                                -- ^ The technology.
                       -> Maybe (Construction, TechnologyOperation) -- ^ The reified technology and its operation, if possible.


-- | Size a technology.
sizeTechnology :: (FTechnology ∈ rs, FYear ∈ rs, FNameplate ∈ rs, FDutyCycle ∈ rs)
               => Technology          -- ^ The technology.
               -> Year                -- ^ The year.
               -> Double              -- ^ The flow [kg/yr].
               -> Set (FieldRec rs)   -- ^ The candidate technologies.
               -> Maybe (FieldRec rs) -- ^ The sized technology, if any.
sizeTechnology tech built demand candidates =
  let
    eligible =
      sortBy (compare `on` (\rec -> (- fYear <: rec, fNameplate <: rec * fDutyCycle <: rec)))
        [
          candidate
        |
          candidate <- S.toList candidates
        , tech  == fTechnology <: candidate
        , built >= fYear       <: candidate
        ]
    year = maximum $ (fYear <:) <$> eligible
    scalable =
      [
        candidate
      |
        candidate <- eligible
      , year     == fYear     <: candidate
      , demand   >= fNameplate <: candidate * fDutyCycle <: candidate
      ]
  in
    trace' (show ("A", tech, built, demand, length eligible, if null scalable then 0 else year, length scalable)) $ if null eligible
      then Nothing
      else Just $ if null scalable
        then head eligible
        else last scalable


-- | Size process inputs and outputs.
sizeInputs :: (FMaterial ∈ rs, FTechnology ∈ rs, FYear ∈ rs, FNameplate ∈ rs)
           => Technology        -- ^ The technology.
           -> Year              -- ^ The year.
           -> Double            -- ^ The flow [kg/yr].
           -> Set (FieldRec rs) -- ^ The candidate inputs.
           -> [FieldRec rs]     -- ^ The inputs.
sizeInputs = sizeInputsOutputs "input costs"


-- | Size process inputs and outputs.
sizeOutputs :: (FMaterial ∈ rs, FTechnology ∈ rs, FYear ∈ rs, FNameplate ∈ rs)
            => Technology        -- ^ The technology.
            -> Year              -- ^ The year.
            -> Double            -- ^ The flow [kg/yr].
            -> Set (FieldRec rs) -- ^ The candidate outputs.
            -> [FieldRec rs]     -- ^ The outputs.
sizeOutputs = sizeInputsOutputs "output costs"


-- | Size process inputs and outputs.
sizeInputsOutputs :: (FMaterial ∈ rs, FTechnology ∈ rs, FYear ∈ rs, FNameplate ∈ rs)
                  => String            -- ^ Which dataset.
                  -> Technology        -- ^ The technology.
                  -> Year              -- ^ The year.
                  -> Double            -- ^ The flow [kg/yr].
                  -> Set (FieldRec rs) -- ^ The candidate inputs/outputs.
                  -> [FieldRec rs]     -- ^ The inputs/outputs.
sizeInputsOutputs message tech built capacity candidates =
  let
    eligible' =
      [
        candidate
      |
        candidate <- S.toList candidates
      , tech      == fTechnology <: candidate
      , built     >= fYear       <: candidate
      ]
  in
    [
      let
        eligible =
          sortBy (compare `on` (\rec -> (- fYear <: rec, fNameplate <: rec)))
            [
              candidate
            |
              candidate <- eligible'
            , material == fMaterial <: candidate
            ]
        year = maximum $ (fYear <:) <$> eligible
        scalable =
          [
            candidate
          |
            candidate <- eligible
          , year     == fYear     <: candidate
          , capacity >= fNameplate <: candidate
          ]
      in
        if null eligible
          then error $ "Missing " ++ message ++ " data for technology \"" ++ show tech ++ "\" and material \"" ++ show material ++ "\" in year " ++ show built ++ "."
          else if null scalable
            then head eligible
            else last scalable
    |
      material <- nub $ (fMaterial <:) <$> eligible'
    ]


-- | Size emissions intensities.
sizeIntensity :: Year                                                          -- ^ The year.
              -> Material                                                      -- ^ The material.
              -> IntensityCube '[]                                             -- ^ The intensity datacube.
              -> [FieldRec '[FMaterial, FUpstreamMaterial, FYear, FIntensity]] -- ^ The intensities.
sizeIntensity year material intensities =
  let
    eligible' =
      [
        candidate
      |
        candidate <- S.toList $ knownKeys (intensities :: IntensityCube '[])
      , material  == fMaterial <: candidate
      , year      >= fYear     <: candidate
      ]
  in
    [
      let
        eligible =
          sortBy (compare `on` (fYear <:))
            [
              candidate
            |
              candidate <- eligible'
            , upstream == fUpstreamMaterial <: candidate
            ]
      in
        if null eligible
          then error $ "Missing intensity data for material \"" ++ show material ++ "\" and upstream material \"" ++ show upstream ++ "\" in year " ++ show year ++ "."
          else let
                 z = last eligible
               in
                 z <+> intensities ! z
    |
      upstream <- nub $ (fUpstreamMaterial <:) <$> eligible'
    ]


-- | Create a technology reifier.
technologyReifier :: ProcessLibrary    -- ^ The process library.
                  -> IntensityCube '[] -- ^ The intensity datacube.
                  -> TechnologyReifier -- ^ The reifier.
technologyReifier processLibrary@ProcessLibrary{..} intensityCube specifics built demand' distance' tech = 
  do -- FIXME: Add interpolation
    let
      demand = abs demand'
    specification <- 
      sizeTechnology
        tech
        built
        demand
        $ knownKeys processCostCube
    let
      costs =  processCostCube ! specification :: FieldRec ProcessCost
      distance = if fProductive <: costs == Central then 0 else distance'
      capacity = maximum [demand / fDutyCycle <: specification, fNameplate <: specification]
      storage = fStorage <: specification * capacity / fNameplate <: specification
      scaleCost cost stretch =
        (cost <: costs + distance * stretch <: costs)
          * (capacity / fNameplate <: specification) ** (fScaling <: costs)
      construction =
            specifics
        <+> fTechnology   =: tech
        <+> fProductive   =: fProductive <: costs
        <+> fYear         =: built
        <+> fLifetime     =: fLifetime <: costs
        <+> fNameplate    =: capacity
        <+> fDutyCycle    =: fDutyCycle <: specification
        <+> fStorage      =: storage
        <+> fLength       =: distance
        <+> fCapitalCost  =: scaleCost fCapitalCost fCapitalCostStretch 
        <+> fFixedCost    =: scaleCost fFixedCost fFixedCostStretch
        <+> fVariableCost =: fVariableCost <: costs + distance * fVariableCostStretch <: costs
    return (construction, operationReifier processLibrary intensityCube construction)


-- | Reify the operation of a technology.
operationReifier :: ProcessLibrary       -- ^ The process library.
                 -> IntensityCube '[]    -- ^ The intensity datacube.
                 -> Construction         -- ^ The constructed technology.
                 -> TechnologyOperation  -- ^ The reified operation
operationReifier ProcessLibrary{..} intensityCube construction pricer =
  let
    specifics = τ construction :: FieldRec '[FInfrastructure, FLocation]
    tech     = fTechnology <: construction
    built    = fYear       <: construction
    capacity = fNameplate  <: construction
    distance = fLength     <: construction
    inputs  = σ (const . (`elem` sizeInputs  tech built capacity (knownKeys processInputCube ))) processInputCube
    outputs = σ (const . (`elem` sizeOutputs tech built capacity (knownKeys processOutputCube))) processOutputCube
  in
    \year output' ->
      let
        output = minimum [abs output', fDutyCycle <: construction * fNameplate <: construction] -- FIXME: Check this.
        specifics' = fInfrastructure =: fInfrastructure <: specifics <+> fYear =: year
        lifetime = fLifetime <: construction
        salvage = maximum [0, fCapitalCost <: construction * fromIntegral (built + lifetime - year - 1) / fromIntegral lifetime]
        capital = if built == year then fCapitalCost <: construction else 0
        fixed = fFixedCost <: construction
        variable = output * fVariableCost <: construction
        impacts =
          mapReduce
            id
            (\key vals -> key <+> fQuantity =: sum ((fQuantity <:) <$> vals) <+> fSale =: sum ((fSale <:) <$> vals))
            [
              (
                    specifics'
                <+> fMaterial       =: upstream
                <+> fImpactCategory =: Upstream
              ,     fQuantity       =: quantity * intensity
                <+> fSale           =: - maximum [0, quantity * intensity] * minimum [0, pricer upstream year]
              )
            |
              rec <- toKnownRecords inputs
            , let material = fMaterial <: rec
            , let rate = fConsumptionRate <: rec + distance * fConsumptionRateStretch <: rec
            , let quantity = output * rate
            , intensities <- sizeIntensity year material intensityCube
            , let upstream = fUpstreamMaterial <: intensities
            , let intensity = (fIntensity <:) intensities
            ]
          ++
          [
                specifics'
            <+> fMaterial       =: fMaterial <: rec
            <+> fImpactCategory =: Consumption
            <+> fQuantity       =: - output * rate
            <+> fSale           =: output * rate * pricer (fMaterial <: rec) year
          |
            rec <- toKnownRecords inputs
          , let rate = fConsumptionRate <: rec + distance * fConsumptionRateStretch <: rec
          ]
          ++
          [
                specifics'
            <+> fMaterial       =: fMaterial <: rec
            <+> fImpactCategory =: Production
            <+> fQuantity       =: output * rate
            <+> fSale           =: - output * rate * pricer (fMaterial <: rec) year
          |
            rec <- toKnownRecords outputs
          , let rate = fProductionRate <: rec + distance * fProductionRateStretch <: rec
          ]
        cash =
          [
            specifics' <+> fCostCategory =: Capital  <+> fSale =: capital
          , specifics' <+> fCostCategory =: Fixed    <+> fSale =: fixed
          , specifics' <+> fCostCategory =: Variable <+> fSale =: variable
          ]
          ++
          [
            specifics' <+> fCostCategory =: cc (fMaterial <: rec) <+> fSale =: fSale <: rec
          |
            rec <- impacts
          , let cc = case fImpactCategory <: rec of
                       Consumption -> Direct
                       Production  -> Direct
                       Upstream    -> Indirect
          ]
      in
        (
              specifics'
          <+> fTechnology =: tech
          <+> fProduction =: (if isProduction (fProductive <: construction) then output else 0      )
          <+> fFlow       =: (if isProduction (fProductive <: construction) then 0      else output')
          <+> fLoss       =: 0
          <+> fSale       =: sum ((fSale <:) <$> cash)
          <+> fSalvage    =: salvage
        , cash
        , impacts
        , output'
        )
