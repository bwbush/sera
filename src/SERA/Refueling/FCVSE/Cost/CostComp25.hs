-- | FCVSE cost data from the spreadsheet "Infrastructure Cost Comparison v23.xlsx".
module SERA.Refueling.FCVSE.Cost.CostComp25 {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Scenario
, scenarios
, sizes
, Capacity
, Cost
, cost
, cost'
) where


import Data.Maybe (fromJust)
import SERA.Refueling.FCVSE.Cost (Cost, Coster)
import SERA.Util.Time (Year(..))


-- | A scenario name.
type Scenario = String


-- | A count of FCVSEs.
type Quantity = Double


-- | List of scenarios and the number of stations associated with each.
scenarios :: [(Scenario, Quantity)]
scenarios =
  [
    ("Low Station Costs"   , 24000000)
  , ("Medium Station Costs",  1000000)
  , ("High Station Costs"  ,    50000)
  ]


-- | The capacity of an FCVSE.
type Capacity = Double


-- | The capacities of the stations in the scenarios.
sizes :: [(Scenario, Capacity)]
sizes =
  [
    ("Small FCVSE" ,  250)
  , ("Medium FCVSE",  500)
  , ("Large FCVSE" , 1500)
  ]


-- | Cost function for a particular scenario.
cost ::
     Scenario -- ^ The scenario.
  -> Coster   -- ^ The cost function.
cost _        _ (Year 2020)      250 = Just 1386480.87647248
cost _        _ (Year 2020)     1500 = Just 3078191.73573182
cost scenario _ (Year 2025) capacity = cost' (fromJust $ lookup scenario scenarios) undefined undefined capacity
cost scenario _ year        capacity = error $ "cost for " ++ scenario ++ " for " ++ show capacity ++ " in " ++ show year ++ " has not been defined"


-- | Cost function for a particular scenario.
cost' ::
     Quantity -- ^ The number of stations.
  -> Coster   -- ^ The cost function.
cost' quantity _ _ capacity = Just $ scaling quantity capacity


-- | The economy of scale function.
scaling ::
     Double -- ^ number of stations [station]
  -> Double -- ^ capacity of stations [kg/day]
  -> Double -- ^ cost of stations [$/station]
scaling quantity capacity =
  anchorPrice
    * (capacity / anchorCapacity)**scalingFactor
    * (quantity / anchorQuantity)**learningCurveFactor
    where
      anchorPrice = 2660480
      anchorCapacity = 450
      anchorQuantity = 25200
      scalingFactor = 0.70594
      learningCurveFactor = -0.1060
