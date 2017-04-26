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

import Control.Lens(makeClassy, ( # ))
import Data.Eq(Eq)
import Data.Digit(Digit, x0, digit, (/+/))
import Data.Int(Int)
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord)
import Prelude(Show, Num((+), (*)))

data TimeAmount =
  TimeAmount {
    _hours :: Int
  , _tenthofhour :: Digit
  } deriving (Eq, Ord, Show)

makeClassy ''TimeAmount

parttimeamount ::
  Digit
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
  in  TimeAmount (f1 + f2 + digit # h) q 

timeAmountBy10 ::
  TimeAmount
  -> Int
timeAmountBy10 (TimeAmount a b) =
  a * 10 + digit # b

instance Monoid TimeAmount where
  mempty =
    zerotimeamount
  mappend =
    addtimeamount
