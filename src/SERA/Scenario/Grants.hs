{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Scenario.Grants (
  allocateGrants
) where


import Data.Daft.DataCube (evaluate)
import Data.Monoid ((<>))
import Data.Daft.Vinyl.FieldCube -- (type (↝), π, σ)
import Data.Daft.Vinyl.FieldRec ((<+>), (=:), (<:))
import Data.Set (Set, findMax, findMin)
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Vinyl.Lens (rput)
import SERA.Refueling.Types -- (FNewCapitalCost, fNewCapitalCost, FNewCapitalIncentives, fNewCapitalIncentives, fNewProductionIncentives, FNewStations, fNewStations, StationDetailCube, FStationID, stationID)
import SERA.Scenario.Types (Cohort(..), FCohort, fCohort, FAnnualGrant, fAnnualGrant, fGrants, FGrantDuration, fGrantDuration, FInitialGrant, fInitialGrant, GrantsCube, fRollover)
import SERA.Types.Fields (fYear, FYear, FRegion, fRegion, region)
import Text.Regex.Posix ((=~))


allocateGrants :: GrantsCube -> StationDetailCube -> StationDetailCube
allocateGrants grants details = allocateGrant grants details `foldMap` ω grants <> details


allocateGrant :: GrantsCube -> StationDetailCube -> FieldRec '[FCohort] -> StationDetailCube
allocateGrant grants details cohort' =
  let
    pattern = cohort $ fCohort <: cohort'
    grants' = σ (const . (== pattern) . cohort . (fCohort <:)) grants
    details' =
      σ (\key rec ->
          region (fRegion <: key) =~ pattern
            && fNewCapitalIncentives <: rec == 0
            && fNewProductionIncentives <: rec == 0
        ) details
    totals :: '[FYear] ↝ '[FNewStations, FNewCapitalCost]
    totals = κ (ω details' :: Set (FieldRec '[FRegion, FStationID])) (\_ recs -> fNewStations =: length recs <+> fNewCapitalCost =: sum ((fNewCapitalCost <:) <$> recs)) details'
    years = ω grants' <> ω totals :: Set (FieldRec '[FYear])
    firstYear = fYear <: findMin years
    lastYear = fYear <: findMax years
--  allocate :: (Double, [(Int, Double, Double)]) -> Int -> (Double, [(Int, Double, Double)])
    allocate (balance, previous) year =
      let
        grant = evaluate grants' $ cohort' <+> fYear =: year
        credit        = maybe 0     (fGrants        <:) grant
        initialGrant  = maybe 0     (fInitialGrant  <:) grant
        annualGrant   = maybe 0     (fAnnualGrant   <:) grant
        grantDuration = maybe 0     (fGrantDuration <:) grant
        rollover      = maybe False (fRollover      <:) grant
        total = evaluate totals $ fYear =: year
        capital  = maybe 0 (fNewCapitalCost <:) total
        stations = maybe 0 (fNewStations    <:) total
        balance' = balance + credit
        availableInitial = minimum [balance', initialGrant * capital]
        balance'' = balance' - availableInitial
        availableAnnual  = minimum [balance'', annualGrant * fromIntegral grantDuration * fromIntegral stations]
        balance''' = if rollover then balance'' - availableAnnual else 0
      in
        (
          balance'''
        , if availableInitial > 0 || availableAnnual > 0
            then (
                       fYear =: year
                   <+> fInitialGrant  =: (if capital  > 0 then availableInitial / capital                                else 0)
                   <+> fAnnualGrant   =: (if stations > 0 then availableAnnual / fromIntegral (grantDuration * stations) else 0)
                   <+> fGrantDuration =: grantDuration
                 ) : previous
            else previous
        )
    allocation :: '[FYear] *↝ '[FInitialGrant, FAnnualGrant, FGrantDuration]
    allocation = fromRecords (snd $ foldl allocate (0, []) [firstYear..lastYear] :: [FieldRec '[FYear, FInitialGrant, FAnnualGrant, FGrantDuration]])
    details'' =
      concat
        [
          rput (Field $ initialGrant * (fNewCapitalCost <: rec) :: ElField FNewCapitalIncentives) rec
          : [
                  fRegion                  =: fRegion <: rec
              <+> fYear                    =: year + i
              <+> fStationID               =: fStationID <: rec
              <+> fNewCapitalCost          =: 0
              <+> fNewInstallationCost     =: 0
              <+> fNewCapitalIncentives    =: 0
              <+> fNewProductionIncentives =: annualGrant
              <+> fNewElectrolysisCapacity =: 0
              <+> fNewPipelineCapacity     =: 0
              <+> fNewOnSiteSMRCapacity    =: 0
              <+> fNewGH2TruckCapacity     =: 0
              <+> fNewLH2TruckCapacity     =: 0
              <+> fRenewableFraction       =: 0
            |
              annualGrant > 0
            , i <- [1..grantDuration]             
            ]
        |
          rec <- toKnownRecords details'
        , let year = fYear <: rec
        , let grant = evaluate allocation $ fYear =: year
        , let initialGrant  = maybe 0 (fInitialGrant  <:) grant
        , let annualGrant   = maybe 0 (fAnnualGrant   <:) grant
        , let grantDuration = maybe 0 (fGrantDuration <:) grant
        , initialGrant > 0
        ]
  in
    ε $ fromRecords details''
