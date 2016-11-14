module SERA.Refueling.FCVSE.Cost.NREL56412 {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Quantity
, capitalCost
, maintenanceCost
, rentCost
, totalFixedOperatingCost
, installationCost
) where


import SERA.Refueling.FCVSE.Cost (Capacity, Cost, Coster)


type Quantity = Double


hscc :: Quantity -> Capacity -> Cost
hscc quantity capacity =
  cost0 * (capacity / capacity0)**alpha * (quantity / quantity0)**beta
    where
      cost0 = 2.8e6     --  $/station
      capacity0 = 450   --  kg/day/station
      quantity0 = 25000 --  kg/day
      alpha = 0.707
      beta = -0.106


capitalCost :: Quantity -> Coster
capitalCost = const . const . ((Just . (0.9 *)) .) . hscc


maintenanceCost :: Capacity -> Cost
maintenanceCost capacity = capacity * 3392.6 * capacity**(-0.609)


rentCost :: Capacity -> Cost
rentCost capacity = capacity * 4624.6 * capacity**(-0.66)


totalFixedOperatingCost :: Capacity -> Cost
totalFixedOperatingCost capacity = capacity * 73125 * capacity**(-0.853)


installationCost :: Quantity -> Coster
installationCost = const . const . ((Just . (0.1 *)) .) . hscc
