{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.SimulatorFlight(
  SimulatorFlight(..)
, HasSimulatorFlight(..)
, dayonlysimulator
) where

import Control.Category((.))
import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Types.Time(Time, HasTime(time), dayonly)
import Data.Aviation.Casr.Logbook.Types.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.Types.TimeAmount(TimeAmount)
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
  , _simulatorTime :: TimeAmount
  , _instrumentsimulatorTime :: TimeAmount
  } deriving (Eq, Ord, Show)   

makeClassy ''SimulatorFlight

instance HasTime SimulatorFlight where
  time =
    simulatorflighttime . time

dayonlysimulator ::
  String
  -> Day
  -> String
  -> [Aviator]
  -> TimeAmount
  -> TimeAmount
  -> SimulatorFlight
dayonlysimulator n d =
  SimulatorFlight
    n
    (dayonly d)
    