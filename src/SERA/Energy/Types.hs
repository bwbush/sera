-----------------------------------------------------------------------------
--
-- Module      :  SERA.Energy.Types
-- Copyright   :  (c) 2016 National Renewable Energy Laboratory
-- License     :  All Rights Reserved
--
-- Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Types for energy computations.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Energy.Types (
-- * Data cubes
  EnergyPriceCube
, FeedstockUsageCube
, CarbonCreditCube
, UtilizationCube
-- * Field types and labels
, HydrogenSource(..)
, FHydrogenSource
, fHydrogenSource
, FeedstockType(..)
, FFeedstockType
, fFeedstockType
, FFeedstockUsage
, fFeedstockUsage
, FUtilization
, fUtilization
, FNonRenewablePrice
, fNonRenewablePrice
, FRenewablePrice
, fRenewablePrice
, FNonRenewableCredit
, fNonRenewableCredit
, FRenewableCredit
, fRenewableCredit
) where


import Data.Daft.Vinyl.FieldCube -- (type (↝), π, σ)
import SERA.Service ()
import SERA.Types.Fields
import SERA.Types.TH (makeField, makeStringField)


$(makeStringField "HydrogenSource" "Hydrogen Source")


$(makeStringField "FeedstockType" "Feedstock")


$(makeField "FeedstockUsage" "Feedstock Usage [/kg]" ''Double)


$(makeField "NonRenewablePrice" "Non-Renewable Price [$]" ''Double)


$(makeField "RenewablePrice" "Renewable Price [$]" ''Double)


$(makeField "NonRenewableCredit" "Carbon Credit (Non-Renewable) [$/kg]" ''Double)


$(makeField "RenewableCredit" "Carbon Credit (Renewable) [$/kg]" ''Double)


$(makeField "Utilization" "Utilization [kg/kg]" ''Double)


type EnergyPriceCube = '[FYear, FFeedstockType] ↝ '[FNonRenewablePrice, FRenewablePrice]


type FeedstockUsageCube = '[FHydrogenSource, FFeedstockType] ↝ '[FFeedstockUsage]


type CarbonCreditCube = '[FHydrogenSource] ↝ '[FNonRenewableCredit, FRenewableCredit]


type UtilizationCube = '[FYear, FRegion] ↝ '[FUtilization]
