{-# LANGUAGE RankNTypes #-}
module Pair (Pair(), cons, car, cdr) where

data Pair a b = Pair (forall c. (a -> b -> c) -> c)

cons :: a -> b -> Pair a b
cons a b = Pair $ \f -> f a b

car :: Pair a b -> a
car (Pair p) = p const

cdr :: Pair a b -> b
cdr (Pair p) = p (\_ b -> b)

instance (Show a, Show b) => Show (Pair a b) where
  show (Pair p) = p $ \a b -> "( " ++ show a ++ ", " ++ show b ++ " )"
