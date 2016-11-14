{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Analysis.Assets {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Assets(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Default.Util (Zero(..))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)
import SERA.Util.Summarization (Summable(..), Totalable(..))


data Assets =
  Assets
    {
      accumulatedCash         :: Double
    , accumulatedPPE          :: Double
    , accumulatedDepreciation :: Double
    , netPPE                  :: Double
    , accumulatedDeferredTaxLosses :: Double
    }
    deriving (Generic, Read, Show)

instance FromJSON Assets

instance ToJSON Assets where
  toJSON = genericToJSON defaultOptions

instance Totalable Assets where
  total Assets{..} = netPPE + accumulatedCash + accumulatedDeferredTaxLosses

instance Summable Assets where
  summation x =
    Assets
    {
      accumulatedCash         = sum' accumulatedCash
    , accumulatedPPE          = sum' accumulatedPPE
    , accumulatedDepreciation = sum' accumulatedDepreciation
    , netPPE                  = sum' netPPE
    , accumulatedDeferredTaxLosses = sum' accumulatedDeferredTaxLosses
    }
      where sum' = sum . flip map x

instance Zero Assets where
  zero = Assets 0 0 0 0 0

instance Tabulatable Assets where
  labels _ =
    [
      "Assets"
    , "    Cumulative Cash [$]"
    , "    Cumulative PP&E [$]"
    , "    Cumulative Depreciation [$]"
    , "    Net PP&E [$]"
    , "    Cumulative Tax Loss Carry-Forward [$]"
    , "  Total Assets [$]"
    ]
  tabulation assets@Assets{..} =
    [
      ""
    , show accumulatedCash
    , show accumulatedPPE
    , show accumulatedDepreciation
    , show netPPE
    , show accumulatedDeferredTaxLosses
    , show $ total assets
    ]
  untabulation = undefined  -- TODO: to be implemented
