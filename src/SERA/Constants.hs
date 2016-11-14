module SERA.Constants {-# DEPRECATED "This module is obsolete and will be refactored or removed." #-} (
  days_per_year
, mmBTU_per_J
, gge_per_J
, galGasoline_per_J
, galDiesel_per_J
, kgH2_per_J
, kWh_per_J
, kgH2PerDay_per_ggePerYear
) where


days_per_year :: Fractional a => a 
days_per_year = 365


mmBTU_per_J :: Fractional a => a
mmBTU_per_J = 1055.87e6


gge_per_J :: Fractional a => a
gge_per_J = 121.3e6


galGasoline_per_J :: Fractional a => a
galGasoline_per_J = 0.114 * mmBTU_per_J -- FIXME: check against GGE value


galDiesel_per_J :: Fractional a => a
galDiesel_per_J = 0.1295 * mmBTU_per_J


kgH2_per_J :: Fractional a => a
kgH2_per_J = 120.1e6


kWh_per_J :: Fractional a => a
kWh_per_J = 3.6e6


kgH2PerDay_per_ggePerYear :: Fractional a => a
kgH2PerDay_per_ggePerYear = kgH2_per_J / gge_per_J / days_per_year
