{-# LANGUAGE RankNTypes #-}
module Number ( Number(), fromChurch, toChurch, zero, plusOne, plus, times
              , expo, minus, equal
              ) where

import Boolean

data Number = Number (forall x. (x -> x) -> x -> x)

fromChurch (Number n) = n (+1) 0
toChurch 0 = zero
toChurch n = plusOne $ toChurch (n - 1)

instance Show Number where
  show = show . fromChurch

zero :: Number
zero = Number $ \f -> id

plusOne :: Number -> Number
plusOne (Number n) = Number $ \f -> f . n f

plus :: Number -> Number -> Number
plus (Number n) (Number m) = n plusOne . m plusOne $ zero

times (Number n) (Number m) = Number $ \f -> n $ m f

expo (Number b) (Number e) = Number $ e b

type NumExtractor = Number -> Boolean -> Number

plusOneMaybe :: NumExtractor -> NumExtractor
plusOneMaybe f n (Boolean b) = f (b (plusOne n) n) true

minusOne :: Number -> Number
minusOne (Number n) = n plusOneMaybe (\n b -> n) zero false


-- Really max(0, a - b), use at your own risk
minus :: Number -> Number -> Number
minus a (Number b) = b minusOne a

equalZero :: Number -> Boolean
equalZero (Number n) = n (\b -> false) true

equal :: Number -> Number -> Boolean
equal a b = equalZero $ plus (minus a b) (minus b a)
