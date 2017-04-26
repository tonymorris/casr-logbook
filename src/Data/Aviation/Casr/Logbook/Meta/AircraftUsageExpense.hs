{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.AircraftUsageExpense(
  AircraftUsageExpense(AircraftUsageExpense)
, HasAircraftUsageExpense(..)
, aircraftUsageCost
) where

import Control.Lens(makeClassy, (^.))
import Data.Aviation.Casr.Logbook.Types.AircraftFlight(HasAircraftFlight, daynight)
import Data.Aviation.Casr.Logbook.Types.TimeAmount(timeAmountBy10)
import Data.Aviation.Casr.Logbook.Types.DayNight(totalDayNight)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show, (*))

data AircraftUsageExpense =
  AircraftUsageExpense {
    _aircraftusageexpenseperhour :: Int
  , _aircraftusageexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftUsageExpense

aircraftUsageCost ::
  HasAircraftFlight s =>
  s
  -> AircraftUsageExpense
  -> Int
aircraftUsageCost fl (AircraftUsageExpense perhour _) =
  let z = totalDayNight (fl ^. daynight)
  in  timeAmountBy10 z * perhour
