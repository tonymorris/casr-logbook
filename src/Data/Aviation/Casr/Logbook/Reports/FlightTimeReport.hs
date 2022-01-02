{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Reports.FlightTimeReport(
  FlightTimeReport(..)
, HasFlightTimeReport(..)
, singleFlightTimeReport
, getFlightTimeReport
) where

import Control.Category((.))
import Control.Lens ( (^.), view, makeClassy )
import Data.Aviation.Casr.Logbook.Types
    ( Engine(Multi, Single),
      TimeAmount,
      Aviator,
      HasAircraft(aircraftRegistration, aircraftType, aircraftEngine),
      HasDayNight(nightDayNight, dayDayNight),
      totalDayNight,
      Command(InCommandInstructing, InCommand, Dual, ICUS),
      HasAircraftFlight(instrumentflightTime, flightaircraft, command,
                        daynight),
      Entry(AircraftFlightEntry),
      Entries(Entries),
      Logbook(Logbook),
      HasInstructionRating(instructionRating),
      getUnderInstructionPic,
      isRAInstruction,
      isGAInstruction )
import Data.Eq(Eq)
import Data.Foldable(foldl')
import Data.Int(Int)
import qualified Data.Map as Map(unionWith, singleton, empty)
import Data.Map(Map)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Prelude(Show, (+))

data FlightTimeReport =
  FlightTimeReport {
    _flightsTotal ::
      Int
  , _hoursTotal ::
      TimeAmount
  , _hoursTotalICUS ::
      TimeAmount
  , _hoursTotalDual ::
      TimeAmount
  , _hoursTotalInCommand ::
      TimeAmount
  , _hoursInAircraftType ::
      Map String (TimeAmount, TimeAmount, TimeAmount, TimeAmount)
  , _hoursInAircraftRegistration ::
      Map String (TimeAmount, TimeAmount, TimeAmount, TimeAmount)
  , _hoursSingleEngine ::
      TimeAmount
  , _hoursSingleEngineICUS ::
      TimeAmount
  , _hoursSingleEngineDual ::
      TimeAmount
  , _hoursSingleEngineInCommand ::
      TimeAmount
  , _hoursMultiEngine ::
      TimeAmount
  , _hoursMultiEngineICUS ::
      TimeAmount
  , _hoursMultiEngineDual ::
      TimeAmount
  , _hoursMultiEngineInCommand ::
      TimeAmount
  , _hoursDay ::
      TimeAmount
  , _hoursDayICUS ::
      TimeAmount
  , _hoursDayDual ::
      TimeAmount
  , _hoursDayInCommand ::
      TimeAmount
  , _hoursInstructing ::
      TimeAmount
  , _hoursGAInstructing ::
      TimeAmount
  , _hoursRAInstructing ::
      TimeAmount
  , _hoursNight ::
      TimeAmount
  , _hoursNightICUS ::
      TimeAmount
  , _hoursNightDual ::
      TimeAmount
  , _hoursNightInCommand ::
      TimeAmount
  , _hoursWithPiC ::
      Map Aviator TimeAmount
  , _hoursInstrument ::
      TimeAmount
  } deriving (Eq, Ord, Show)

makeClassy ''FlightTimeReport

instance Semigroup FlightTimeReport where
  FlightTimeReport ft1 tl1 tli1 tld1 tlc1 tp1 rg1 se1 sei1 sed1 sec1 me1 mei1 med1 mec1 dy1 dyi1 dyd1 dyc1 ins1 insr1 insg1 nt1 nti1 ntd1 ntc1 wpc1 is1 <> FlightTimeReport ft2 tl2 tli2 tld2 tlc2 tp2 rg2 se2 sei2 sed2 sec2 me2 mei2 med2 mec2 dy2 dyi2 dyd2 dyc2 ins2 insr2 insg2 nt2 nti2 ntd2 ntc2 wpc2 is2 =
    FlightTimeReport
      (ft1 + ft2)
      (tl1 <> tl2)
      (tli1 <> tli2)
      (tld1 <> tld2)
      (tlc1 <> tlc2)
      (Map.unionWith mappend tp1 tp2)
      (Map.unionWith mappend rg1 rg2)
      (se1 <> se2)
      (sei1 <> sei2)
      (sed1 <> sed2)
      (sec1 <> sec2)
      (me1 <> me2)
      (mei1 <> mei2)
      (med1 <> med2)
      (mec1 <> mec2)
      (dy1 <> dy2)
      (dyi1 <> dyi2)
      (dyd1 <> dyd2)
      (dyc1 <> dyc2)
      (ins1 <> ins2)
      (insr1 <> insr2)
      (insg1 <> insg2)
      (nt1 <> nt2)
      (nti1 <> nti2)
      (ntd1 <> ntd2)
      (ntc1 <> ntc2)
      (Map.unionWith mappend wpc1 wpc2)
      (is1 <> is2)

instance Monoid FlightTimeReport where
  mempty =
    FlightTimeReport
      0
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
  mappend =
    (<>)

singleFlightTimeReport ::
  Entry a b c d
  -> FlightTimeReport
singleFlightTimeReport (AircraftFlightEntry fl _) =
  let hoursdaynight = totalDayNight (fl ^. daynight)
      icus x =
        case fl ^. command of
          ICUS _ ->
            x
          Dual _ ->
            mempty
          InCommand ->
            mempty
          InCommandInstructing _ ->
            mempty
      dual x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            x
          InCommand ->
            mempty
          InCommandInstructing _ ->
            mempty
      comd x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            mempty
          InCommand ->
            x
          InCommandInstructing _ ->
            mempty
      instr x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            mempty
          InCommand ->
            mempty
          InCommandInstructing _ ->
            x
      instrRA x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            mempty
          InCommand ->
            mempty
          InCommandInstructing a ->
            if isRAInstruction (view instructionRating a) then x else mempty
      instrGA x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            mempty
          InCommand ->
            mempty
          InCommandInstructing a ->
            if isGAInstruction (view instructionRating a) then x else mempty
      hoursmap k =
        Map.singleton k (hoursdaynight, icus hoursdaynight, dual hoursdaynight, comd hoursdaynight)
      singleengine x =
        case fl ^. flightaircraft . aircraftEngine of
          Single ->
            x
          Multi ->
            mempty
      multiengine x =
        case fl ^. flightaircraft . aircraftEngine of
          Single ->
            mempty
          Multi ->
            x
      totalhoursday =
        fl ^. daynight . dayDayNight
      totalhoursnight =
        fl ^. daynight . nightDayNight
      pic x =
        case getUnderInstructionPic (fl ^. command) of
          Just a ->
            Map.singleton a x
          Nothing ->
            Map.empty
  in  FlightTimeReport
        1
        hoursdaynight
        (icus hoursdaynight)
        (dual hoursdaynight)
        (comd hoursdaynight)
        (hoursmap (fl ^. flightaircraft . aircraftType))
        (hoursmap (fl ^. flightaircraft . aircraftRegistration))
        (singleengine hoursdaynight)
        (singleengine (icus hoursdaynight))
        (singleengine (dual hoursdaynight))
        (singleengine (comd hoursdaynight))
        (multiengine hoursdaynight)
        (multiengine (icus hoursdaynight))
        (multiengine (dual hoursdaynight))
        (multiengine (comd hoursdaynight))
        totalhoursday
        (icus hoursdaynight)
        (dual hoursdaynight)
        (comd hoursdaynight)
        (instr hoursdaynight)
        (instrRA hoursdaynight)
        (instrGA hoursdaynight)
        totalhoursnight
        (icus totalhoursnight)
        (dual totalhoursnight)
        (comd totalhoursnight)
        (pic hoursdaynight)
        (fl ^. instrumentflightTime)
singleFlightTimeReport _ =
  mempty

getFlightTimeReport ::
  Logbook a b c d
  -> FlightTimeReport
getFlightTimeReport (Logbook _ (Entries es)) =
  foldl' (\a -> mappend a . singleFlightTimeReport) mempty es
