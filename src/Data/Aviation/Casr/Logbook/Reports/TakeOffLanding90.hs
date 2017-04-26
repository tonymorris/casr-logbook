{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Reports.TakeOffLanding90(
  TakeOffLanding90(..)
, HasTakeOffLanding90(..)
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Types(FlightPoint)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Time(Day)
import Prelude(Show)

data TakeOffLanding90 =
  TakeOffLanding90 {
    _takeoff1 ::
      FlightPoint
  , _takeoff2 ::
      FlightPoint
  , _takeoff3 ::
      FlightPoint
  , _landing1 ::
      FlightPoint
  , _landing2 ::
      FlightPoint
  , _landing3 ::
      FlightPoint
  , _currency90 ::
      Day
  }
  deriving (Eq, Ord, Show)

makeClassy ''TakeOffLanding90
