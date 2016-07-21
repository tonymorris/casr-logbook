{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.SimulatorFlight(
  SimulatorFlight(..)
, HasSimulatorFlight(..)
, dayonlysimulator
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Time(Time, dayonly)
import Data.Aviation.Casr.Logbook.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.TimeAmount(TimeAmount)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.String(String)
import Data.Time(Day)
import Prelude(Show)

data SimulatorFlight =
  SimulatorFlight {
    _simulatorflightname :: String
  , _simulatorflighttime :: Time
  , _simulatortype :: String
  , _simulatorothercrew :: [Aviator]
  , _instrumentsimulatorTimeAmount :: TimeAmount
  } deriving (Eq, Ord, Show)   

makeClassy ''SimulatorFlight

dayonlysimulator ::
  String
  -> Day
  -> String
  -> [Aviator]
  -> TimeAmount
  -> SimulatorFlight
dayonlysimulator n d =
  SimulatorFlight
    n
    (dayonly d)
    