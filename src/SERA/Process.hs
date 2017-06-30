{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process (
  productions
, deliveries
, processes
, pathways
) where


import Data.Daft.Vinyl.FieldCube (σ, ω)
import Data.Daft.Vinyl.FieldRec ((<:))
import Data.Set (Set, toList)
import Data.Vinyl.Derived (FieldRec)
import SERA.Process.Types -- FIXME


type Technologies = Set (FieldRec '[FTechnology])


filterTechnologiesByProductive :: (Productive -> Bool) -> ProcessLibrary -> [Technology]
filterTechnologiesByProductive f ProcessLibrary{..} =
  fmap (fTechnology <:)
    . toList
    $ (ω $ σ (const $ f . (fProductive <:)) processCostCube :: Technologies)


productions :: ProcessLibrary -> [Technology]
productions = filterTechnologiesByProductive isProduction


deliveries :: ProcessLibrary -> [Technology]
deliveries = filterTechnologiesByProductive $ not . isProduction


processes :: ProcessLibrary -> [Technology]
processes = filterTechnologiesByProductive $ const True


type Pathways = Set (FieldRec '[FPathway])


pathways :: ProcessLibrary -> [Pathway]
pathways ProcessLibrary{..} = fmap (fPathway <:) . toList $ (ω pathwayCube :: Pathways)
