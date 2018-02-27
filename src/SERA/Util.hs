{-# LANGUAGE FlexibleContexts    #-}


module SERA.Util (
  combineRecs
) where


import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Function (on)


-- TODO: Move to `daft` package.


combineRecs field operation x y = field =: on operation (field <:) x y
