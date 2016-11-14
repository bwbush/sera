{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.Taxation {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Taxation(..)
, unknownTaxation
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..), nan)
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Util.Summarization (Summable(..), Totalable(..), sumEach)


data Taxation =
  Taxation {
    earningsBeforeInterestTaxesAndDepreciation :: Double
  , interestOnOutstandingDebt                  :: Double
  , equipmentDepreciation                      :: [Double]
  , taxableIncome                              :: Double
  , taxesBeforeDeferment                       :: Double
  , taxLossCarryForward                        :: [Double]
  , taxesDue                                   :: Double
  }
    deriving (Generic, Read, Show)

instance FromJSON Taxation

instance ToJSON Taxation where
  toJSON = genericToJSON defaultOptions

instance Totalable Taxation where
  total Taxation{..} = taxesDue

instance Summable Taxation where
  summation x =
    Taxation
    {
      earningsBeforeInterestTaxesAndDepreciation = sum' earningsBeforeInterestTaxesAndDepreciation
    , interestOnOutstandingDebt                  = sum' interestOnOutstandingDebt
    , equipmentDepreciation                      = sum'' equipmentDepreciation
    , taxableIncome                              = sum' taxableIncome
    , taxesBeforeDeferment                       = sum' taxesBeforeDeferment
    , taxLossCarryForward                        = sum'' taxLossCarryForward
    , taxesDue                                   = sum' taxesDue
    }
      where
        sum' = sum . flip map x
        sum'' = sumEach . flip map x

instance Zero Taxation where
  zero = Taxation 0 0 [0] 0 0 (replicate 7 0) 0

instance Tabulatable Taxation where
  labels Taxation{..} =
    [
      "Earnings before Interest, Taxes, and Depreciation [$]"
    , "Interest on Outstanding Debt [$]"
    , "Equipment Depreciation [$]"
    , "Taxable Income [$]"
    , "  Taxes before Deferment [$]"
    , "  Remaining Taxes Loss Carry-Forward from 1 Year Ago [$]"
    ]
    ++
    [
      "  Remaining Taxes Loss Carry-Forward from " ++ show i ++ " Years Ago [$]"
    |
      i <- [2..length taxLossCarryForward]
    ]
    ++
    [
      "Taxes Due [$]"
    ]
  tabulation Taxation{..} =
    [
      show earningsBeforeInterestTaxesAndDepreciation
    , show interestOnOutstandingDebt
    , show $ head equipmentDepreciation
    , show taxableIncome
    , show taxesBeforeDeferment
    ]
    ++
    map show taxLossCarryForward
    ++
    [
      show taxesDue
    ]
  untabulation = undefined  -- TODO: to be implemented


unknownTaxation :: Taxation
unknownTaxation = Taxation nan nan [nan] nan nan (replicate 7 nan) nan
