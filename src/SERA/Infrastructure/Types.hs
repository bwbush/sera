{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}


module SERA.Infrastructure.Types
-- FIXME
where


import SERA.Types.TH (makeField, makeStringType)


$(makeStringType "Technology")

-- $(makeField "Technology" "Technology Type"   ''Technology)
$(makeField "Distance"   "Distance [km]"     ''Double    )
--  $(makeField "Capacity"   "Capacity [kg/day]" ''Double    )
--  $(makeField "Year"       "Year"              ''Int       )


{-
name

distance
capacity
year
capacity
Capital [$]
Fixed Operating [$/yr]
Variable Operating [$/kg]
Feedstock
Yield






Technology
name
capacity
distance
year
property
  capital
  fixed
  vari
  oper
  feedstock
  output
yield


infrastructure
name
centralized
lifetime

costs
name
year
capacity
distance
capitalCost
fixedOperating
variableOperatingCost
efficiency

input
name
year
capacity
distance
feedstock
rate

output
name
year
capacity
distance
emission
rate
-}
