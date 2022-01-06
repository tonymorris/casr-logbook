{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.AircraftFlight(
  AircraftFlight(..)
, HasAircraftFlight(..)
, icusonlyflight
, noif_icusonlyflight
, dualonlyflight
, noif_dualonlyflight
, commandonlyflight
, noif_commandonlyflight
, instructionflight
, noif_instructionflight
, aeronauticalHours
) where

import Control.Category((.))
import Control.Lens ( view, makeClassy )
import Data.Aviation.Casr.Logbook.Types.Aircraft(Aircraft, HasAircraft(aircraft))
import Data.Aviation.Casr.Logbook.Types.Command
    ( Command(InCommandInstructing, InCommand, Dual, ICUS),
      isAeronauticalHours )
import Data.Aviation.Casr.Logbook.Types.DayNight
    ( DayNight(DayNight), HasDayNight(dayNight) )
import Data.Aviation.Casr.Logbook.Types.FlightPath(FlightPath, HasFlightPath(flightPath))
import Data.Aviation.Casr.Logbook.Types.Instruction ( Instruction )
import Data.Aviation.Casr.Logbook.Types.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.Types.TimeAmount
    ( TimeAmount, zerotimeamount )
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

instance HasAircraft AircraftFlight where
  aircraft =
    flightaircraft . aircraft

instance HasDayNight AircraftFlight where
  dayNight =
    daynight . dayNight

instance HasFlightPath AircraftFlight where
  flightPath =
    flightpath . flightPath

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

instructionflight ::
  String
  -> Aircraft
  -> Instruction
  -> DayNight
  -> FlightPath
  -> TimeAmount
  -> AircraftFlight
instructionflight n a s t p i =
  AircraftFlight
    n
    a
    (InCommandInstructing s)
    t
    p
    []
    i

noif_instructionflight ::
  String
  -> Aircraft
  -> Instruction
  -> DayNight
  -> FlightPath
  -> AircraftFlight
noif_instructionflight n a s t p =
  AircraftFlight
    n
    a
    (InCommandInstructing s)
    t
    p
    []
    zerotimeamount

aeronauticalHours ::
  AircraftFlight
  -> DayNight
aeronauticalHours fl =
  if isAeronauticalHours (view command fl)
    then
      view dayNight fl
    else
      DayNight zerotimeamount zerotimeamount
