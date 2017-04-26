{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.SimulatorFlightExpense(
  SimulatorFlightExpense(SimulatorFlightExpense)
, HasSimulatorFlightExpense(..)
, simulatorFlightCost
) where

import Control.Lens(makeClassy, (^.))
import Data.Aviation.Casr.Logbook.Types.SimulatorFlight(HasSimulatorFlight, instrumentsimulatorTime)
import Data.Aviation.Casr.Logbook.Types.TimeAmount(timeAmountBy10)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show, (*))

data SimulatorFlightExpense =
  SimulatorFlightExpense {
    _simulatorflightexpenseperhour :: Int
  , _simulatorflightexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightExpense

simulatorFlightCost ::
  HasSimulatorFlight s =>
  s
  -> SimulatorFlightExpense
  -> Int
simulatorFlightCost sf (SimulatorFlightExpense perhour _) =
  let z = sf ^. instrumentsimulatorTime
  in  timeAmountBy10 z * perhour
