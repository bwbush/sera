{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.OperatingExpenses {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  OperatingExpenses(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Finance.Analysis.FeedstockUtilityExpenses (FeedstockUtilityExpenses)
import SERA.Util.Summarization (Summable(..), Totalable(..))


data OperatingExpenses =
  OperatingExpenses {
    feedstockAndUtilities    :: FeedstockUtilityExpenses
  , labor                    :: Double
  , maintenance              :: Double
  , rent                     :: Double
  , propertyInsurance        :: Double
  , licensingAndPermitting   :: Double
  , sellingAndAdministrative :: Double
  }
    deriving (Generic, Read, Show)

instance FromJSON OperatingExpenses

instance ToJSON OperatingExpenses where
  toJSON = genericToJSON defaultOptions

instance Totalable OperatingExpenses where
  total OperatingExpenses{..} = total feedstockAndUtilities + labor + maintenance + rent + propertyInsurance + licensingAndPermitting + sellingAndAdministrative

instance Summable OperatingExpenses where
  summation x =
    OperatingExpenses
    {
      feedstockAndUtilities    = summation $ map feedstockAndUtilities x
    , labor                    = sum' labor
    , maintenance              = sum' maintenance
    , rent                     = sum' rent
    , propertyInsurance        = sum' propertyInsurance
    , licensingAndPermitting   = sum' licensingAndPermitting
    , sellingAndAdministrative = sum' sellingAndAdministrative
    }
      where sum' = sum . flip map x

instance Zero OperatingExpenses where
  zero = OperatingExpenses zero 0 0 0 0 0 0

instance Tabulatable OperatingExpenses where
  labels OperatingExpenses{..} =
    [
      "Operating Expenses"
    ]
    ++
    labels feedstockAndUtilities
    ++
    [
      "    Labor [$]"
    , "    Maintenance [$]"
    , "    Rent [$]"
    , "    Property Insurance [$]"
    , "    Licensing and Permitting [$]"
    , "    Selling and Administrative [$]"
    , "  Total Operating Expenses [$]"
    ]
  tabulation operatingExpenses@OperatingExpenses{..} =
    [
      ""
    ]
    ++
    tabulation feedstockAndUtilities
    ++
    [
      show labor
    , show maintenance
    , show rent
    , show propertyInsurance
    , show licensingAndPermitting
    , show sellingAndAdministrative
    , show $ total operatingExpenses
    ]
  untabulation = undefined  -- TODO: to be implemented
