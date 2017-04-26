{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Reports.FlightTimeReport(
  FlightTimeReport(..)
, HasFlightTimeReport(..)
, singleFlightTimeReport
, getFlightTimeReport
) where

import Control.Category((.))
import Control.Lens(makeClassy, (^.))
import Data.Aviation.Casr.Logbook.Types
  (
    TimeAmount
  , Aviator
  , Logbook(Logbook)
  , Command(ICUS, Dual, InCommand)
  , Engine(Single, Multi)
  , Entry(AircraftFlightEntry)
  , Entries(Entries)
  , aircraftEngine
  , aircraftRegistration
  , aircraftType
  , instrumentflightTime
  , getInstructingPic
  , flightaircraft
  , command
  , daynight
  , dayDayNight
  , nightDayNight
  , totalDayNight
  )
import Data.Eq(Eq)
import Data.Foldable(foldl')
import Data.Int(Int)
import qualified Data.Map as Map(unionWith, singleton, empty)
import Data.Map(Map)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
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
  FlightTimeReport ft1 tl1 tli1 tld1 tlc1 tp1 rg1 se1 sei1 sed1 sec1 me1 mei1 med1 mec1 dy1 dyi1 dyd1 dyc1 nt1 nti1 ntd1 ntc1 wpc1 is1 `mappend` FlightTimeReport ft2 tl2 tli2 tld2 tlc2 tp2 rg2 se2 sei2 sed2 sec2 me2 mei2 med2 mec2 dy2 dyi2 dyd2 dyc2 nt2 nti2 ntd2 ntc2 wpc2 is2 =
    FlightTimeReport
      (ft1 + ft2)
      (tl1 `mappend` tl2)
      (tli1 `mappend` tli2)
      (tld1 `mappend` tld2)
      (tlc1 `mappend` tlc2)
      (Map.unionWith mappend tp1 tp2)
      (Map.unionWith mappend rg1 rg2)
      (se1 `mappend` se2)
      (sei1 `mappend` sei2)
      (sed1 `mappend` sed2)
      (sec1 `mappend` sec2)
      (me1 `mappend` me2)
      (mei1 `mappend` mei2)
      (med1 `mappend` med2)
      (mec1 `mappend` mec2)
      (dy1 `mappend` dy2)
      (dyi1 `mappend` dyi2)
      (dyd1 `mappend` dyd2)
      (dyc1 `mappend` dyc2)
      (nt1 `mappend` nt2)
      (nti1 `mappend` nti2)
      (ntd1 `mappend` ntd2)
      (ntc1 `mappend` ntc2)
      (Map.unionWith mappend wpc1 wpc2)
      (is1 `mappend` is2)

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
      dual x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            x
          InCommand ->
            mempty            
      comd x =
        case fl ^. command of
          ICUS _ ->
            mempty
          Dual _ ->
            mempty
          InCommand ->
            x
      hoursmap k =
        Map.singleton k (hoursdaynight, (icus hoursdaynight), (dual hoursdaynight), (comd hoursdaynight))
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
        case getInstructingPic (fl ^. command) of
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
        (icus totalhoursday)
        (dual totalhoursday)
        (comd totalhoursday)
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
