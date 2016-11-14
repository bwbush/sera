{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module SERA.Finance.Project {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Project(..)
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)


data Project =
  Project {
    projectId   :: String
  , projectName :: String
  }
  deriving (Generic, Read, Show)

instance FromJSON Project

instance ToJSON Project where
  toJSON = genericToJSON defaultOptions

instance Eq Project where
  Project x _  == Project y _ = x == y

instance Ord Project where
  Project x _ `compare` Project y _ = compare x y

instance Tabulatable Project where
  labels = const ["Project ID", "Project Name"]
  tabulation Project{..} = [projectId, projectName]
  untabulation [projectId, projectName] = Project{..}
  untabulation _ = undefined
