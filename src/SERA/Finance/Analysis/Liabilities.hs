{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.Liabilities {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Liabilities(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Util.Summarization (Summable(..), Totalable(..))


data Liabilities =
  Liabilities
    {
      accumulatedDebt              :: Double
    }
    deriving (Generic, Read, Show)

instance FromJSON Liabilities

instance ToJSON Liabilities where
  toJSON = genericToJSON defaultOptions

instance Totalable Liabilities where
  total Liabilities{..} = accumulatedDebt

instance Summable Liabilities where
  summation x =
    Liabilities
    {
      accumulatedDebt              = sum' accumulatedDebt
    }
      where sum' = sum . flip map x

instance Zero Liabilities where
  zero = Liabilities 0

instance Tabulatable Liabilities where
  labels _ =
    [
      "Liabilities"
    , "    Cumulative Debt [$]"
    , "  Total Liabilities [$]"
    ]
  tabulation liabilities@Liabilities{..} =
    [
      ""
    , show accumulatedDebt
    , show $ total liabilities
    ]
  untabulation = undefined  -- TODO: to be implemented
