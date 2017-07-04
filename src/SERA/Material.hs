{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Material (
  makePricer
) where


import Data.Daft.DataCube (evaluate)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Vinyl.Derived (FieldRec)
import SERA.Material.Types (fMaterial, fPrice, PriceCube, Pricer)
import SERA.Types (fYear)


makePricer :: Ord (FieldRec key) => PriceCube key -> FieldRec key -> Pricer
makePricer priceCube key material year =
  maybe 0 (fPrice <:) 
    . evaluate priceCube
    $ fMaterial =: material <+> fYear =: year <+> key
