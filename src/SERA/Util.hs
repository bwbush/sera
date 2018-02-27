{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}


module SERA.Util (
  combineRecs
, extractKey
, extractValue
) where


import Data.Daft.DataCube (knownKeys)
import Data.Daft.Vinyl.FieldRec ((=:), (<:))
import Data.Function (on)

import qualified Data.Set as S (empty, insert, map)


-- TODO: Move to `daft` package.


combineRecs field operation x y = field =: on operation (field <:) x y


extractKey field = S.map field . knownKeys


extractValue field = foldr (S.insert . field) S.empty
