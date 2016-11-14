{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.FeedstockUtilityExpenses {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  FeedstockUtilityExpenses(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Util.Summarization (Summable(..), Totalable(..))


data FeedstockUtilityExpenses =
  FeedstockUtilityExpenses {
    electricity       :: Double
  , naturalGas        :: Double
  , deliveredHydrogen :: Double
  }
    deriving (Generic, Read, Show)

instance FromJSON FeedstockUtilityExpenses

instance ToJSON FeedstockUtilityExpenses where
  toJSON = genericToJSON defaultOptions

instance Totalable FeedstockUtilityExpenses where
  total FeedstockUtilityExpenses{..} = electricity + naturalGas + deliveredHydrogen

instance Summable FeedstockUtilityExpenses where
  summation x =
    FeedstockUtilityExpenses
    {
      electricity       = sum' electricity
    , naturalGas        = sum' naturalGas
    , deliveredHydrogen = sum' deliveredHydrogen
    }
      where sum' = sum . flip map x

instance Zero FeedstockUtilityExpenses where
  zero = FeedstockUtilityExpenses 0 0 0

instance Tabulatable FeedstockUtilityExpenses where
  labels _ =
    [
      "      Electricity [$]"
    , "      Natural Gas [$]"
    , "      Delivered Hydrogen [$]"
    , "    Total Feedstock & Utilities [$]"
    ]
  tabulation feedstockUtilityExpenses@FeedstockUtilityExpenses{..} =
    [
      show electricity
    , show naturalGas
    , show deliveredHydrogen
    , show $ total feedstockUtilityExpenses
    ]
  untabulation = undefined  -- TODO: to be implemented
