{-# LANGUAGE RankNTypes #-}
module Boolean where

data Boolean = Boolean (forall x. x -> x -> x)
true :: Boolean
true = Boolean $ \a b -> a

false :: Boolean
false = Boolean $ \a b -> b

switch (Boolean b) = b

fromBoolean :: Boolean -> Bool
fromBoolean (Boolean a) = a True False

instance Show Boolean where
  show = show . fromBoolean

and :: Boolean -> Boolean -> Boolean
and (Boolean a) b = a b false

or :: Boolean -> Boolean -> Boolean
or (Boolean a) b = a true b

not :: Boolean -> Boolean
not (Boolean a) = a false true
