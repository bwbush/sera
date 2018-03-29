{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process.Reification.Technology
-- FIXME
where


import Data.Daft.DataCube (knownKeys)
import Data.Daft.Vinyl.FieldCube ((!), σ, toKnownRecords)
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
import SERA.Types.Fields -- FIXME
import SERA.Types.Records -- FIXME

import qualified Data.Set as S (findMax, findMin, null, toList)


type TechnologyOperation = Year -> Double -> (Flow, [Cash], [Impact], Double)


type TechnologyReifier = FieldRec '[FInfrastructure, FLocation] -> Year -> Double -> Double -> Technology -> Maybe (Construction, TechnologyOperation)


selectTechnology :: (FTechnology ∈ rs, FYear ∈ rs, FNameplate ∈ rs, FDutyCycle ∈ rs) => Technology -> Year -> Double -> Set (FieldRec rs) -> Maybe (FieldRec rs)
selectTechnology tech built demand candidates =
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


selectTechnology' :: (FMaterial ∈ rs, FTechnology ∈ rs, FYear ∈ rs, FNameplate ∈ rs) => String -> Technology -> Year -> Double -> Set (FieldRec rs) -> [FieldRec rs]
selectTechnology' message tech built capacity candidates =
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


selectIntensity :: Year -> Material -> IntensityCube '[] -> [FieldRec '[FMaterial, FUpstreamMaterial, FYear, FIntensity]]
selectIntensity year material intensities =
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


technologyReifier :: ProcessLibrary -> IntensityCube '[] -> Pricer -> TechnologyReifier
technologyReifier ProcessLibrary{..} intensityCube pricer specifics built demand' distance' tech = 
  do -- FIXME: Add interpolation
    let
      demand = abs demand'
    specification <- 
      selectTechnology
        tech
        built
        demand
        $ knownKeys processCostCube
    let
      costs =  processCostCube ! specification :: FieldRec ProcessCost
      distance = if fProductive <: costs == Central then 0 else distance'
      capacity = maximum [demand / fDutyCycle <: specification, fNameplate <: specification]
      scaleCost cost stretch =
        (cost <: costs + distance * stretch <: costs)
          * (capacity / fNameplate <: specification) ** (fScaling <: costs)
      construction =
            specifics
        <+> fTechnology   =: tech
        <+> fProductive   =: fProductive <: costs
        <+> fYear         =: built
        <+> fLifetime     =: fLifetime <: costs
        <+> fNameplate     =: capacity
        <+> fDutyCycle    =: fDutyCycle <: specification
        <+> fLength       =: distance
        <+> fCapitalCost  =: scaleCost fCapitalCost fCapitalCostStretch 
        <+> fFixedCost    =: scaleCost fFixedCost fFixedCostStretch
        <+> fVariableCost =: fVariableCost <: costs + distance * fVariableCostStretch <: costs
      inputs =
        σ (\kex _ -> kex `elem` selectTechnology'
                                  "input cost"
                                  tech
                                  built
                                  capacity
                                  (knownKeys processInputCube)
          ) processInputCube
      outputs =
        σ (\kex _ -> kex `elem` selectTechnology'
                                  "output cost"
                                  tech
                                  built
                                  capacity
                                  (knownKeys processOutputCube)
          ) processOutputCube
    return
      (
        construction
      , \year output' ->
        let
          output = minimum [abs output', fDutyCycle <: construction * fNameplate <: construction] -- FIXME: Check this.
          specifics' = fInfrastructure =: fInfrastructure <: specifics <+> fYear =: year
          lifetime = fLifetime <: costs
          salvage = maximum [0, fCapitalCost <: construction * fromIntegral (built + lifetime - year) / fromIntegral lifetime]
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
              , intensities <- selectIntensity year material intensityCube
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
            <+> fProduction =: (if isProduction (fProductive <: costs) then output else 0      )
            <+> fFlow       =: (if isProduction (fProductive <: costs) then 0      else output')
            <+> fLoss       =: 0
            <+> fSale       =: (sum $ (fSale <:) <$> cash)
            <+> fSalvage    =: salvage
          , cash
          , impacts
          , output'
          )
      )


findMin :: String -> Set a -> a -- FIXME: Move this to the error monad.
findMin message x
  | S.null x  = error message
  | otherwise = S.findMin x


findMax :: String -> Set a -> a -- FIXME: Move this to the error monad.
findMax message x
  | S.null x  = error message
  | otherwise = S.findMax x
