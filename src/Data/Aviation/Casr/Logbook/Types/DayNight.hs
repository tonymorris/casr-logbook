{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.DayNight(
  DayNight(..)
, HasDayNight(..)
, day
, night
, totalDayNight
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Types.TimeAmount(TimeAmount(TimeAmount), zerotimeamount)
import Data.Digit(DecDigit)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Monoid(mappend)
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
  -> DecDigit
  -> DayNight
day h p =
  DayNight (TimeAmount h p) zerotimeamount

night ::
  Int
  -> DecDigit
  -> DayNight
night h p =
  DayNight zerotimeamount (TimeAmount h p)

totalDayNight ::
  DayNight
  -> TimeAmount
totalDayNight (DayNight d n) =
  d `mappend` n
