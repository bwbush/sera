{-# LANGUAGE DeriveGeneric   #-}


module SERA.Configuration.FileLocations {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  FileLocations(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import GHC.Generics (Generic)


data FileLocations =
  FileLocations
    {
      demandsFile                :: FilePath
    , stationsFile               :: FilePath
    , financialAnalysisDirectory :: FilePath
    , summarizationFile          :: FilePath
    }
    deriving (Generic, Read, Show)

instance FromJSON FileLocations

instance ToJSON FileLocations where
  toJSON = genericToJSON defaultOptions
