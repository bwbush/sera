module SERA.Util.Wilder (
  Wilder(..)
, wilder
, tamer
, tameWith
) where


import Control.Arrow (first)


data Wilder a = Wild | Tame {tame :: a}

instance Eq a => Eq (Wilder a) where
  Tame x == Tame y = x == y
  _      == _      = True     -- FIXME: This is inconsistend with the `Ord` instance.

instance Ord a => Ord (Wilder a) where
  Tame x `compare` Tame y = x `compare` y
  Tame _ `compare` Wild   = LT
  Wild   `compare` Tame _ = GT
  Wild   `compare` Wild   = EQ

instance Read a => Read (Wilder a) where
  readsPrec _ ('*' : xs) = [(Wild, xs)]
  readsPrec n xs         = first Tame <$> readsPrec n xs

instance Show a => Show (Wilder a) where
  show Wild     = "*"
  show (Tame x) = show x

instance Functor Wilder where
  f `fmap` Tame x = Tame $ f x
  _ `fmap` Wild   = Wild

instance Applicative Wilder where
  pure = Tame
  Tame f <*> x = fmap f x
  Wild   <*> _ = Wild

tameWith :: a -> Wilder a -> a
tameWith _ (Tame x) = x
tameWith x Wild     = x


tamer :: Wilder a -> Maybe a
tamer (Tame x) = Just x
tamer Wild     = Nothing


wilder :: Maybe a -> Wilder a
wilder (Just x) = Tame x
wilder Nothing  = Wild
