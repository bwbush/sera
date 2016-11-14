{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.Expenses {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Expenses(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Finance.Analysis.OperatingExpenses (OperatingExpenses)
import SERA.Finance.Analysis.Taxation (Taxation)
import SERA.Util.Summarization (Summable(..), Totalable(..))


data Expenses =
  Expenses {
    operatingExpenses :: OperatingExpenses
  , taxation          :: Taxation
  }
    deriving (Generic, Read, Show)

instance FromJSON Expenses

instance ToJSON Expenses where
  toJSON = genericToJSON defaultOptions

instance Totalable Expenses where
  total Expenses{..} = total operatingExpenses + total taxation

instance Summable Expenses where
  summation x =
    Expenses
    {
      operatingExpenses = summation' operatingExpenses
    , taxation          = summation' taxation
    }
      where
        summation' :: Summable a => (Expenses -> a) -> a
        summation' = summation . flip map x

instance Zero Expenses where
  zero = Expenses zero zero

instance Tabulatable Expenses where
  labels Expenses{..} =
    labels operatingExpenses
    ++
    labels taxation
  tabulation Expenses{..} =
    tabulation operatingExpenses
    ++
    tabulation taxation
  untabulation = undefined  -- TODO: to be implemented
