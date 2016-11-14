{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}


-- | Representation of time.
module SERA.Util.Time {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Year(..)
, makeYears
) where


import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Monoid (Sum(Sum))
import Data.Monoid.Action (Action(act))
import Data.Table (Tabulatable(..))
import GHC.Generics (Generic)


-- | Years.
newtype Year = Year {unYear :: Int}
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Year

instance ToJSON Year where
  toJSON = genericToJSON defaultOptions

instance Action (Sum Int) Year where
  act (Sum delta) (Year year) = Year (year + delta) 

instance Tabulatable Year where
  labels _ = ["Year"]
  tabulation = return . show . unYear
  untabulation = Year . read . head


-- | Make a list of years.
makeYears ::
     [Int]  -- ^ Numerical values of the years.
  -> [Year] -- ^ The years.
makeYears = map Year
