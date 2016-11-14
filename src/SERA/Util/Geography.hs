{-# LANGUAGE RecordWildCards #-}


-- | Representation of geographic regions.
module SERA.Util.Geography {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Region(..)
, mkRegion
) where


import Data.Table (Tabulatable(..))


-- | A geographic region.
data Region =
  Region {
    regionId :: String   -- ^ The unique ID for the region.
  , regionName :: String -- ^ The name of the region
  }
    deriving (Read, Show)

mkRegion :: String -> Region
mkRegion = flip Region ""

instance Eq Region where
  Region x _  == Region y _ = x == y

instance Ord Region where
  Region x _ `compare` Region y _ = compare x y

instance Tabulatable Region where
  labels _ = ["Region ID", "Region Name"]
  tabulation Region{..} = [regionId, regionName]
  untabulation [regionId, regionName] = Region{..}
  untabulation _ = undefined
