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
-- | Types and accessors for data fields.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}


module SERA.Types.Fields (
-- * Data types
  CostCategory(..)
, ImpactCategory(..)
, Position(..)
-- * Field types
, FConsumption
, FCostCategory
, FFlow
, FFuelConsumption
, FImpactCategory
, FLoss
, FNetPrice
, FNonFuelConsumption
, FPosition
, FProduction
, FQuantity
, FSales
, FSalvage
, FTotalCost
-- * FIeld accessors
, fConsumption
, fCostCategory
, fFlow
, fFuelConsumption
, fImpactCategory
, fLoss
, fNetPrice
, fNonFuelConsumption
, fPosition
, fProduction
, fQuantity
, fSales
, fSalvage
, fTotalCost
) where


import SERA.Material.Types (Material)
import SERA.Types.TH (makeField, makeStringField)


data CostCategory =
    Capital
  | Salvage
  | Fixed
  | Variable
  | Direct   Material
  | Indirect Material
    deriving (Eq, Ord, Read, Show)


data ImpactCategory =
    Consumption
  | Production
  | Upstream
    deriving (Eq, Ord, Read, Show)


$(makeField       "Consumption"        "Demand [kg]"                      ''Double        )
$(makeField       "CostCategory"       "Cost Component"                   ''CostCategory  )
$(makeField       "Flow"               "Flow [kg]"                        ''Double        )
$(makeField       "FuelConsumption"    "Fueling-Station Demand [kg]"      ''Double        )
$(makeField       "ImpactCategory"     "Disposition"                      ''ImpactCategory)
$(makeField       "Loss"               "Loss [kg]"                        ''Double        )
$(makeField       "NetPrice"           "Price [$/kg]"                     ''Double        )
$(makeField       "NonFuelConsumption" "Non-Fueling-Station Demand [kg]"  ''Double        )
$(makeStringField "Position"           "Position"                                         )
$(makeField       "Production"         "Production [kg]"                  ''Double        )
$(makeField       "Quantity"           "Quantity [unit]"                  ''Double        )
$(makeField       "Sales"              "Sales [$]"                        ''Double        )
$(makeField       "Salvage"            "Salvage Value [$]"                ''Double        )
$(makeField       "TotalCost"          "Cost [kg]"                        ''Double        )
