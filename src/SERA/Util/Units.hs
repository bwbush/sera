module SERA.Util.Units {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  CurrencyUnits(..)
, EnergyUnits(..)
) where


-- | Units of measure for currency.
data CurrencyUnits =
    -- | Nominal dollars.
    Dollars
  deriving (Eq, Ord, Read, Show)


-- | Units of measure for energy.
data EnergyUnits =
    -- | Millions of BTUs.
    MillionBtu
  | -- | Kilowatt-hours.
    Kwh
  | -- | Gallons of gasoline equivalent.
    Gge
  deriving (Eq, Ord, Read, Show)
