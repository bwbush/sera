-- | FCVSE costs.
module SERA.Refueling.FCVSE.Cost {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Station(..)
, Capacity
, Cost
, Coster
, makeCoster
) where


import Data.Tuple.Util (curry3)
import SERA.Util.Time (Year(..))

import qualified Data.Map as M


-- | A FCVSE refueling station.
newtype Station = Station String
  deriving (Eq, Ord, Read, Show)


-- | FCVSE cost.
type Cost = Double


-- | FCVSE capacity.
type Capacity = Double


-- | Function for computing FCVSE cost.
type Coster =
     Station    -- ^ The FCVSE type.
  -> Year       -- ^ The year.
  -> Capacity   -- ^ The FCVSE capacity.
  -> Maybe Cost -- ^ The FCVSE cost, if any.


-- | Cost data, which is a list of FCVSE name, year, capacity, and cost.
type CostData = [(String, Int, Double, Double)]


-- | Make a cost function using cost data.
makeCoster :: CostData -> Coster
makeCoster =
  curry3 . flip M.lookup . M.fromList . map makeEntry
    where
      makeEntry :: (String, Int, Double, Double) -> ((Station, Year, Capacity), Cost)
      makeEntry (station, year, capacity, cost) = ((Station station, Year year, capacity), cost)
