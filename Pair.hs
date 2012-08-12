{-# LANGUAGE RankNTypes #-}
module Pair where

data Pair a b = Pair (forall c. (a -> b -> c) -> c)

cons :: a -> b -> Pair a b
cons a b = Pair $ \f -> f a b

car :: Pair a b -> a
car (Pair p) = p (\a b -> a)

cdr :: Pair a b -> b
cdr (Pair p) = p (\a b -> b)

instance (Show a, Show b) => Show (Pair a b) where
  show (Pair p) = p $ \a b -> "( " ++ (show a) ++ ", " ++ (show b) ++ " )"
