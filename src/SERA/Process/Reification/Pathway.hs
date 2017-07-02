{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process.Reification.Pathway
-- FIXME
where


import Data.Daft.Vinyl.FieldCube (σ, toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Tuple.Util (fst3, snd3, trd3)
import Data.Vinyl.Derived (ElField(..), FieldRec)
import Data.Vinyl.Lens (rput)
import SERA.Infrastructure.Types -- FIXME
import SERA.Network.Types -- FIXME
import SERA.Process.Reification.Technology (TechnologyReifier)
import SERA.Process.Types -- FIXME
import SERA.Types (Year)


type PathwayOperation = Year -> Double -> ([Flow], [Cash], [Impact])


type PathwayReifier = FieldRec '[FInfrastructure, FFrom, FTo] -> Pathway -> Year -> Double -> Double -> Maybe ([Construction], PathwayOperation)


transmissionReifier :: ProcessLibrary -> TechnologyReifier -> PathwayReifier
transmissionReifier ProcessLibrary{..} reifyTechnology specifics path built capacity distance = 
  do
    let
      candidate key val = path == fPathway <: key && fTransmission <: val
      specify rec =
            fInfrastructure =: (Infrastructure . ((show (fStage <: rec) ++) . (':' :)) . infrastructure $ fInfrastructure <: specifics)
        <+> fFrom           =: fFrom <: specifics
        <+> fTo             =: (if fExtended <: rec then fTo <: specifics else fFrom <: specifics)
    constructions <-
      sequence
        [
          reifyTechnology (specify rec) (fTechnology <: rec) built capacity distance
        |
          rec <- toKnownRecords $ σ candidate pathwayCube
        ]
    return
      (
        [
          if fFrom <: rec == fTo <: rec
            then rput (Field 0 :: ElField FLength) rec
            else rec
        |
          rec <- fst <$> constructions
        ]
      , \year flow ->
        let
          results =
            [
              operate year flow -- FIXME: include losses
            |
              operate <- snd <$> constructions
            ]
        in
          (
            fst3 <$> results
          , concat $ snd3 <$> results
          , concat $ trd3 <$> results
          )
      )
