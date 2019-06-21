{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.TimeAmount(
  TimeAmount(..)
, HasTimeAmount(..)
, parttimeamount
, zerotimeamount
, addtimeamount
, timeAmountBy10
) where

import Data.Bool(Bool, bool)
import Data.Maybe(fromMaybe)
import Data.Ord((>))
import Data.Semigroup(Semigroup((<>)))
import Prelude(Integral, divMod, mod)
import Control.Lens(makeClassy, (^?), over, _1, ( # ))
import Data.Eq(Eq)
import Data.Digit(DecDigit, x0, x1, integralDecimal)
import Data.Int(Int)
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord)
import Prelude(Show, Num((+), (*)))

data TimeAmount =
  TimeAmount {
    _hours :: Int
  , _tenthofhour :: DecDigit
  } deriving (Eq, Ord, Show)

makeClassy ''TimeAmount

parttimeamount ::
  DecDigit
  -> TimeAmount
parttimeamount =
  TimeAmount 0

zerotimeamount ::
  TimeAmount
zerotimeamount =
  TimeAmount 0 x0

addtimeamount ::
  TimeAmount 
  -> TimeAmount
  -> TimeAmount
TimeAmount f1 p1 `addtimeamount` TimeAmount f2 p2 =
  let (h, q) = p1 /+/ p2
  in  TimeAmount (f1 + f2 + integralDecimal # (h :: DecDigit)) q 

timeAmountBy10 ::
  TimeAmount
  -> Int
timeAmountBy10 (TimeAmount a b) =
  a * 10 + integralDecimal # b

instance Semigroup TimeAmount where
  (<>) =
    addtimeamount

instance Monoid TimeAmount where
  mempty =
    zerotimeamount
  mappend =
    (<>)

---- belongs in digit package

mod10 ::
  Integral a =>
  a
  -> DecDigit
mod10 n =
  let r = n `mod` 10
  in fromMaybe (mod10 r) (r ^? integralDecimal)

addDecDigit ::
  DecDigit
  -> DecDigit
  -> (Bool, DecDigit)
addDecDigit a b =
  let (x, r) =
        (integralDecimal # a + integralDecimal # b) `divMod` 10
  in  (x > 0, mod10 (r :: Int))

(/+/) ::
  DecDigit
  -> DecDigit
  -> (DecDigit, DecDigit)
(/+/) a b =
  over _1 (bool x0 x1) (addDecDigit a b)
