{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Time(
  Time(..)
, HasTime(..)
, dayonly
, dayandtime
, timeofday'
) where

import Control.Category ( Category((.)) )
import Control.Lens(makeClassy, Traversal', _Just)
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

timeofday' ::
  HasTime c =>
  Traversal' c TimeOfDay
timeofday' =
  timeofday . _Just

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
