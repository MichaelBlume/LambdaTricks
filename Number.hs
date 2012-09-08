{-# LANGUAGE RankNTypes #-}
module Number ( Number(), composeN, fromChurch, toChurch, zero, plusOne, plus
              , times, expo, minus, equal, div, mod,
              ) where

import Boolean
import Prelude hiding (div, mod)

data Number = Number (forall x. (x -> x) -> x -> x)
composeN (Number n) = n

fromChurch (Number n) = n (+1) 0
toChurch 0 = zero
toChurch n = plusOne $ toChurch (n - 1)

instance Show Number where
  show = show . fromChurch

zero :: Number
zero = Number $ const id

plusOne :: Number -> Number
plusOne (Number n) = Number $ \f -> f . n f

plus :: Number -> Number -> Number
plus (Number n) (Number m) = n plusOne . m plusOne $ zero

times (Number n) (Number m) = Number $ \f -> n $ m f

expo (Number b) (Number e) = Number $ e b

type NumExtractor = Number -> Boolean -> Number

plusOneMaybe :: NumExtractor -> NumExtractor
plusOneMaybe f n b = f (switch b (plusOne n) n) true

minusOne :: Number -> Number
minusOne (Number n) = n plusOneMaybe const zero false


-- Really max(0, a - b), use at your own risk
minus :: Number -> Number -> Number
minus a (Number b) = b minusOne a

equalZero :: Number -> Boolean
equalZero (Number n) = n (const false) true

equal :: Number -> Number -> Boolean
equal a b = equalZero $ plus (minus a b) (minus b a)

apply x f = f x

addMod base f quot rem = apply (plusOne rem) $ \rem' -> switch (equal rem' base) (f (plusOne quot) zero) (f quot rem')

getDiv :: (Number -> Number -> x) -> Number -> Number -> x
getDiv f (Number num) div = num (addMod div) f zero zero

div :: Number -> Number -> Number
div = getDiv const

mod :: Number -> Number -> Number
mod = getDiv $ \_ x -> x
