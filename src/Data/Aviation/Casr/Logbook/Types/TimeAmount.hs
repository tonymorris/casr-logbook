{-# OPTIONS_GHC -Wall #-}
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

import Data.Semigroup(Semigroup((<>)))
import Control.Lens(makeClassy, ( # ))
import Data.Eq(Eq)
import Data.Digit(DecDigit, x0, integralDecimal, addDecDigit')
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
  let (h, q) = p1 `addDecDigit'` p2
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
