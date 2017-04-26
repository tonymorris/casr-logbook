{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Time(
  Time(..)
, HasTime(..)
, dayonly
, dayandtime
) where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Time(Day, TimeOfDay)
import Prelude(Show)

data Time =
  Time {
    _daytime :: Day
  , _timeofday :: Maybe TimeOfDay
  } deriving (Eq, Ord, Show)

makeClassy ''Time

dayonly ::
  Day
  -> Time
dayonly d =
  Time d Nothing

dayandtime ::
  Day
  -> TimeOfDay
  -> Time
dayandtime d t =
  Time d (Just t)
