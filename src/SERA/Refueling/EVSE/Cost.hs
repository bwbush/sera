-- | EVSE costs.
module SERA.Refueling.EVSE.Cost {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Charger(..)
, Cost
, Coster
, CostData
, makeCoster
) where


import SERA.Util.Time (Year(..))

import qualified Data.Map as M


-- | A EVSE refueling station.
newtype Charger = Charger String
  deriving (Eq, Ord, Read, Show)


-- | EVSE cost.
type Cost = Double


-- | Function for computing EVSE cost.
type Coster =
     Charger    -- ^ The EVSE type.
  -> Year       -- ^ The year.
  -> Maybe Cost -- ^ The EVSE cost, if any.


-- | Cost data, which is a list of EVSE name year, and cost.
type CostData = [(String, Int, Double)]


-- | Make a cost function using cost data.
makeCoster :: CostData -> Coster
makeCoster =
  curry . flip M.lookup . M.fromList . map makeEntry
    where
      makeEntry (charger, year, cost) = ((Charger charger, Year year), cost)
