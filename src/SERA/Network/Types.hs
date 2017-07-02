{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Network.Types (
-- * Data types
  Location(..)
, Zone(..)
-- * Field types
, FLocation
, FZone
, FX
, FY
, FSale
, FRent
, FFrom
, FTo
, FLength
, FTerritory
-- * Field accessors
, fLocation
, fZone
, fX
, fY
, fSale
, fRent
, fFrom
, fTo
, fLength
, fTerritory
-- * Types
, Network(..)
-- * Data cubes
, NodeCube
, LinkCube
, ExistingCube
, TerritoryCube
, ZoneCube
) where


import Data.Daft.Vinyl.FieldCube (type (*↝))
import SERA.Process.Types (FCapacity, FCost, FDelivery, FProductive, FTransmission, FYield)
import SERA.Types (FFraction, FYear)
import SERA.Types.TH (makeField, makeStringField)


$(makeStringField "Location"    "Network ID"                 )
$(makeField       "From"        "From Node ID"     ''Location)
$(makeField       "To"          "To Node ID"       ''Location)
$(makeField       "X"           "X"                ''Double  )
$(makeField       "Y"           "Y"                ''Double  )
$(makeField       "Length"      "Length [km]"      ''Double  )
$(makeField       "Sale"        "Cost [$]"         ''Double  )
$(makeField       "Rent"        "Cost [$/yr]"      ''Double  )
$(makeStringField "Territory"   "Territory"                  )
$(makeStringField "Zone"        "Zone"                       )


data Network =
  Network
  {
    nodeCube :: NodeCube
  , linkCube :: LinkCube
  , existingCube :: ExistingCube
  , territoryCube :: TerritoryCube
  , zoneCube :: ZoneCube '[FLocation]
  }
    deriving (Eq, Ord, Show)


type NodeCube = '[FLocation] *↝ '[FX, FY, FProductive, FSale, FRent]


type LinkCube = '[FLocation] *↝ '[FFrom, FTo, FLength, FSale, FRent, FTransmission, FDelivery]


type ExistingCube = '[FLocation] *↝ '[FYear, FCapacity, FYield, FCost]


type TerritoryCube = '[FTerritory, FLocation] *↝ '[FFraction]


type ZoneCube key = (FZone ': key) *↝ '[FFraction]
