{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.Revenues {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Revenues(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Util.Summarization (Summable(..), Totalable(..))


data Revenues =
  Revenues {
    sales               :: Double
  , productionIncentive :: Double
  , incidentalRevenue   :: Double
  , fuelPrepayments     :: Double
  , consumerDiscounts   :: Double
  , creditCardFees      :: Double
  , salesTaxes          :: Double
  , roadTaxes           :: Double
  }
    deriving (Generic, Read, Show)

instance FromJSON Revenues

instance ToJSON Revenues where
  toJSON = genericToJSON defaultOptions

instance Totalable Revenues where
  total Revenues{..} =
    sales
      + productionIncentive
      + incidentalRevenue
      + fuelPrepayments
      + consumerDiscounts
      + creditCardFees
      + salesTaxes
      + roadTaxes

instance Summable Revenues where
  summation x =
    Revenues
    {
      sales               = sum' sales
    , productionIncentive = sum' productionIncentive
    , incidentalRevenue   = sum' incidentalRevenue
    , fuelPrepayments     = sum' fuelPrepayments
    , consumerDiscounts   = sum' consumerDiscounts
    , creditCardFees      = sum' creditCardFees
    , salesTaxes          = sum' salesTaxes
    , roadTaxes           = sum' roadTaxes
    }
      where sum' = sum . flip map x

instance Zero Revenues where
  zero = Revenues 0 0 0 0 0 0 0 0

instance Tabulatable Revenues where
  labels _ =
    [
      "Revenues (Annual)"
    , "    Sales [$]"
    , "    Production Incentive [$]"
    , "    Incidental Revenue [$]"
    , "    Fuel Prepayments [$]"
    , "    Consumer Discounts [$]"
    , "    Credit Card Fees [$]"
    , "    Sales Taxes [$]"
    , "    Road Taxes [$]"
    , "  Total Revenue [$]"
    ]
  tabulation revenues@Revenues{..} =
    [
      ""
    , show sales
    , show productionIncentive
    , show incidentalRevenue
    , show fuelPrepayments
    , show consumerDiscounts
    , show creditCardFees
    , show salesTaxes
    , show roadTaxes
    , show $ total revenues
    ]
  untabulation = undefined  -- TODO: to be implemented
