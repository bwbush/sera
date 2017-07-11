{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module SERA.Process.Reification.Pathway
-- FIXME
where


import Control.Arrow (first)
import Data.Daft.Vinyl.FieldCube (σ, toKnownRecords)
import Data.Daft.Vinyl.FieldRec ((=:), (<:), (<+>))
import Data.Tuple.Util (fst3, snd3, trd3)
import Data.Vinyl.Derived (ElField(..))
import Data.Vinyl.Lens (rput)
import SERA.Infrastructure.Types -- FIXME
import SERA.Network.Types -- FIXME
import SERA.Process.Reification.Technology (TechnologyReifier)
import SERA.Process.Types -- FIXME
import SERA.Types (Year)


type PathwayOperation = Year -> Double -> ([Flow], [Cash], [Impact])


type PathwayReifier = Year -> Double -> Double -> (Infrastructure, Path) -> Pathway -> Maybe ([Construction], PathwayOperation)


transmissionReifier :: ProcessLibrary -> TechnologyReifier -> PathwayReifier
transmissionReifier =
  pathwayReifier
    $ const


deliveryReifier :: ProcessLibrary -> TechnologyReifier -> PathwayReifier
deliveryReifier =
  pathwayReifier
    $ \transmission delivery -> not transmission && delivery


pathwayReifier :: (Bool -> Bool -> Bool) -> ProcessLibrary -> TechnologyReifier -> PathwayReifier
pathwayReifier candidate ProcessLibrary{..} reifyTechnology built capacity distance (label, GenericPath{..}) path = 
  do
    let
      candidate' key val = path == fPathway <: key && candidate (fTransmission <: val) (fDelivery <: val)
      stages = toKnownRecords $ σ candidate' pathwayCube
      extendedStage = fStage <: head (filter (\r -> fExtended <: r) stages)
      relabel rec i = fInfrastructure =: (Infrastructure . ((++ show (i :: Int)) . (++ ".") . (++ show (fStage <: rec)) . (++ " #")) $ infrastructure label)
    constructions <-
      sequence
        $ [
            first (rput (Field 0 :: ElField FLength))
              <$> reifyTechnology (relabel rec 0 <+> fLocation =: sourceId) built capacity distance (fTechnology <: rec)
          |
            rec <- stages
          , fStage <: rec < extendedStage
          ]
          ++
          [
            first (rput (Field d :: ElField FLength))
              <$> reifyTechnology (relabel rec i <+> fLocation =: l) built capacity d (fTechnology <: rec)
          |
            rec <- stages
          , fStage <: rec == extendedStage
          , (i, (l, d)) <- zip [1..] linkIds
          ]
          ++
          [
            first (rput (Field 0 :: ElField FLength))
              <$> reifyTechnology (relabel rec 0 <+> fLocation =: sinkId) built capacity distance (fTechnology <: rec)
          |
            rec <- stages
          , fStage <: rec > extendedStage
          ]
    return
      (
        fst <$> constructions
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
