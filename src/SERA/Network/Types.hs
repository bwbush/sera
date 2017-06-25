{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Network.Types (
-- * Data types
  Location
, Node
, Link
, Zone
-- * Field types
, FLocation
, FNode
, FLink
, FZone
-- * Field accessors
, fLocation
, fNode
, fLink
, fZone
-- * Data cubes
, ZoneCube
) where


import Data.Daft.Vinyl.FieldCube (type (*↝))
import SERA.Types (FFraction, FYear)
import SERA.Types.TH (makeField, makeStringField)


$(makeStringField "Location" "Network ID")
$(makeStringField "Node"     "Node ID"   )
$(makeStringField "Link"     "Link ID"   )
$(makeStringField "Zone"     "Zone"      )


type ZoneCube key = (FZone ': key) *↝ '[FFraction]
