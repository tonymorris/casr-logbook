{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.DayNight(
  DayNight(..)
, HasDayNight(..)
, day
, night
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.TimeAmount(TimeAmount(TimeAmount), zerotimeamount)
import Data.Digit(Digit)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Prelude(Show)

data DayNight =
  DayNight {
    _dayDayNight :: TimeAmount
  , _nightDayNight :: TimeAmount
  } deriving (Eq, Ord, Show)

makeClassy ''DayNight

day ::
  Int
  -> Digit
  -> DayNight
day h p =
  DayNight (TimeAmount h p) zerotimeamount

night ::
  Int
  -> Digit
  -> DayNight
night h p =
  DayNight zerotimeamount (TimeAmount h p)
