{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.AircraftFlight(
  AircraftFlight(..)
, icusonlyflight
, noif_icusonlyflight
, dualonlyflight
, noif_dualonlyflight
, commandonlyflight
, noif_commandonlyflight
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Aircraft(Aircraft)
import Data.Aviation.Casr.Logbook.Command(Command(ICUS, Dual, InCommand))
import Data.Aviation.Casr.Logbook.DayNight(DayNight)
import Data.Aviation.Casr.Logbook.FlightPath(FlightPath)
import Data.Aviation.Casr.Logbook.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.TimeAmount(TimeAmount, zerotimeamount)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data AircraftFlight =
  AircraftFlight {
    _aircraftflightname :: String
  , _flightaircraft :: Aircraft
  , _command :: Command
  , _daynight :: DayNight
  , _flightpath :: FlightPath
  , _flightothercrew :: [Aviator]
  , _instrumentflightTime :: TimeAmount
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftFlight

icusonlyflight ::
  String
  -> Aircraft
  -> Aviator
  -> DayNight
  -> FlightPath
  -> TimeAmount
  -> AircraftFlight
icusonlyflight n a v t p i =
  AircraftFlight
    n
    a
    (ICUS v)
    t
    p
    []
    i

noif_icusonlyflight ::
  String
  -> Aircraft
  -> Aviator
  -> DayNight
  -> FlightPath
  -> AircraftFlight
noif_icusonlyflight n a v t p =
  icusonlyflight
    n
    a
    v
    t
    p
    zerotimeamount

dualonlyflight ::
  String
  -> Aircraft
  -> Aviator
  -> DayNight
  -> FlightPath
  -> TimeAmount
  -> AircraftFlight
dualonlyflight n a v t p i =
  AircraftFlight
    n
    a
    (Dual v)
    t
    p
    []
    i

noif_dualonlyflight ::
  String
  -> Aircraft
  -> Aviator
  -> DayNight
  -> FlightPath
  -> AircraftFlight
noif_dualonlyflight n a v t p =
  dualonlyflight
    n
    a
    v
    t
    p
    zerotimeamount

commandonlyflight ::
  String
  -> Aircraft
  -> DayNight
  -> FlightPath
  -> TimeAmount
  -> AircraftFlight
commandonlyflight n a t p i =
  AircraftFlight
    n
    a
    InCommand
    t
    p
    []
    i

noif_commandonlyflight ::
  String
  -> Aircraft
  -> DayNight
  -> FlightPath
  -> AircraftFlight
noif_commandonlyflight n a t p =
  commandonlyflight
    n
    a
    t
    p
    zerotimeamount
