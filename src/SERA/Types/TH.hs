{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}


module SERA.Types.TH (
  makeField
, makeStringField
, makeStringType
, quotedStringTypes
) where


import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withText)
import Data.Char (toLower)
import Data.Default (Default)
import Data.String.ToString (toString)
import Data.Vinyl.Derived (SField(..))
import GHC.Generics (Generic)
import Language.Haskell.TH


-- | Whether to quote string types in 'Show' and 'Read' instances.
quotedStringTypes :: Bool
quotedStringTypes = False


makeField :: String -> String -> Name -> Q [Dec]
makeField name label typeName =
  do
    let
      fieldName = mkName $ 'F' : name
      instanceName = mkName $ 'f' : name
    return
      [
        TySynD fieldName [] $ AppT (AppT (PromotedTupleT 2) (LitT $ StrTyLit label)) (ConT typeName)
      , SigD instanceName $ AppT (ConT ''SField) (ConT fieldName)
      , FunD instanceName [Clause [] (NormalB $ ConE 'SField) []]
      ]


makeStringField :: String -> String -> String -> Q [Dec]
makeStringField name label typName =
  liftM2 (++) (makeStringType typName) (makeField name label $ mkName typName)


makeStringType :: String -> Q [Dec]
makeStringType name =
  do
    let
      fieldName = mkName $ toLower (head name) : tail name
      typeName = mkName name
    details <-
        [d| -- FIXME: Consider rewriting as non-template code.
          instance Read $(conT typeName) where
            readsPrec
              | quotedStringTypes = (fmap (first $(conE typeName)) .) . readsPrec
              | otherwise         = const $ return . (\x -> (x, [])) . $(conE typeName)
          
          instance Show $(conT typeName) where
            show
              | quotedStringTypes = show . $(varE fieldName)
              | otherwise         = $(varE fieldName)
          
          instance FromJSON $(conT typeName) where
            parseJSON = (withText . show $ mkName name) $ return . $(conE typeName) . toString
          
          instance ToJSON $(conT typeName) where
            toJSON = toJSON . $(varE fieldName)
        |]
    return
      $ (NewtypeD [] typeName [] (RecC typeName [(fieldName, NotStrict, ConT ''String)]) [''Default, ''Eq, ''Generic, ''Ord])
      :  details
