module AEO.Types {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  Region(..)
, parseRegion
, Sector(..)
, parseSector
, Source(..)
, parseSource
) where


import Data.Char (isSpace)


-- | Census divisions, see <http://www.eia.gov/forecasts/aeo/pdf/f1.pdf>.
data Region =
    -- | The United States.
    UnitedStates
  | -- | The New England census division.
    NewEngland
  | -- | The Middle Atlantic census division.
    MiddleAtlantic
  | -- | The East North Central census division.
    EastNorthCentral
  | -- | The West North Central census division.
    WestNorthCentral
  | -- | The South Atlantic census division.
    SouthAtlantic
  | -- | The East South Central census division.
    EastSouthCentral
  | -- | The West South Central census division.
    WestSouthCentral
  | -- | The Mountain census division.
    Mountain
  | -- | The Pacific census division.
    Pacific
    deriving (Bounded, Enum, Eq, Ord, Read, Show)


-- | Parse the name of a region.
parseRegion ::
     String -- ^ The name.
  -> Region -- ^ The region.
parseRegion "United States"           = UnitedStates
parseRegion "New England - 01"        = NewEngland
parseRegion "Middle Atlantic - 02"    = MiddleAtlantic
parseRegion "East North Central - 03" = EastNorthCentral
parseRegion "West North Central - 04" = WestNorthCentral
parseRegion "South Atlantic - 05"     = SouthAtlantic
parseRegion "East South Central - 06" = EastSouthCentral
parseRegion "West South Central - 07" = WestSouthCentral
parseRegion "Mountain - 08"           = Mountain
parseRegion "Pacific - 09"            = Pacific
parseRegion other                     = error $ "parseRegion: " ++ other


-- | Energy usage sectors.
data Sector =
    -- | Residential energy usage.
    Residential
  | -- | Commercial energy usage.
    Commercial
  | -- | Industrial energy usage.
    Industrial
  | -- | Transportation energy usage.
    Transportation
  | -- | Electric power energy usage.
    ElectricPower
  | -- | All energy usage.
    AllUses
    deriving (Enum, Eq, Ord, Read, Show)


parseSector :: String -> Sector
parseSector = read . filter (not . isSpace)


-- | Energy sources.
data Source =
    -- | Propane.
    Propane
  | -- | Distillate fuel oil (e.g., diesel).
    DistillateFuelOil
  | -- | Residual fuel oil.
    ResidualFuelOil
  | -- | Natural gas.
    NaturalGas
  | -- | Electricity.
    Electricity
  | -- | Metallurgical coal.
    MetallurgicalCoal
  | -- | Other industrial coal.
    OtherIndustrialCoal
  | -- | Coal converted to liquid fuel.
    CoalToLiquids
  | -- | E85 (e.g., 85% ethanol blend with gasoline).
    E85
  | -- | Gasoline for motor fuel.
    MotorGasoline
  | -- | Jet fuel.
    JetFuel
  | -- | Coal for steam production.
    SteamCoal
    deriving (Enum, Eq, Ord, Read, Show)


parseSource :: String -> Source
parseSource = read . filter (not . isSpace)
