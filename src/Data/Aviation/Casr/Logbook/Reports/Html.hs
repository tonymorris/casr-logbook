{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Casr.Logbook.Reports.Html(
    htmlFlightPointDay
  , htmlTakeOffLanding90
  , takeofflanding
  , takeoffslandings90
  , htmlSimulatorTimeReport
  , htmlFlightTimeReport
) where

import Control.Category((.))
import Control.Lens((^.), _Wrapped)
import Data.Aviation.Casr.Logbook.Types.AircraftFlight
    ( flightpath )
import Data.Aviation.Casr.Logbook.Types.Entry
    ( Entry(AircraftFlightEntry) )
import Data.Aviation.Casr.Logbook.Types.FlightPath
    ( flightStart, flightIntermediate, flightEnd )
import Data.Aviation.Casr.Logbook.Types.FlightPoint
    ( landingTime, point, FlightPoint )
import Data.Aviation.Casr.Logbook.Types.Logbook
    ( logbookentries, Logbook )
import Data.Aviation.Casr.Logbook.Types.Time ( daytime )
import Data.Aviation.Casr.Logbook.Html.Html(
    htmlTimeAmount
  , htmlAviatorShort
  )
import Data.Aviation.Casr.Logbook.Reports.FlightTimeReport
    ( FlightTimeReport,
      HasFlightTimeReport(hoursInstrument, hoursWithPiC,
                          hoursGA3Instructing, hoursGA2Instructing, hoursGA1Instructing,
                          hoursGAInstructing, hoursRASeniorInstructing,
                          hoursRAJuniorInstructing, hoursRAInstructing, hoursNightInCommand,
                          hoursNightDual, hoursNightICUS, hoursNight, hoursDayInCommand,
                          hoursDayDual, hoursDayICUS, hoursDay, hoursMultiEngineInCommand,
                          hoursMultiEngineDual, hoursMultiEngineICUS, hoursMultiEngine,
                          hoursSingleEngineInCommand, hoursSingleEngineDual,
                          hoursSingleEngineICUS, hoursSingleEngine,
                          hoursInAircraftRegistration, hoursInAircraftType, hoursInAircraftTypeVariant, hoursInstructing,
                          hoursTotalInCommand, hoursTotalDual, hoursTotalICUS, hoursTotal,
                          flightsTotal) )
import Data.Aviation.Casr.Logbook.Reports.SimulatorTimeReport
    ( HasSimulatorTimeReport(hoursTotalSimulator,
                             hoursInstrumentSimulator),
      SimulatorTimeReport )
import Data.Aviation.Casr.Logbook.Reports.TakeOffLanding90
    ( HasTakeOffLanding90(currency90, landing3, landing2, landing1,
                          takeoff3, takeoff2, takeoff1),
      TakeOffLanding90(TakeOffLanding90) )
import Data.Foldable(foldr)
import Data.Function(flip, ($))
import qualified Data.Map as Map(foldrWithKey)
import Data.List(sortBy, (++))
import Data.Maybe(Maybe(Nothing, Just))
import Data.Monoid(mempty)
import Data.Ord(comparing, min)
import Data.String(fromString)
import qualified Data.Text as Text(pack)
import Data.Time(addDays)
import Lucid(
    class_
  , span_
  , ul_
  , li_
  , div_
  , h3_
  , href_
  , id_
  , a_
  , ol_
  , Html
  )
import Prelude(show)

htmlFlightPointDay ::
  FlightPoint
  -> Html ()
htmlFlightPointDay p =
  let j = p ^. landingTime . daytime
  in  do  span_ [class_ "currencyflightpointday"] . fromString . show $ j
          " "
          span_ [class_ "currencyflightpointpoint"] . fromString $ p ^. point

htmlTakeOffLanding90 ::
  Logbook a b c d
  -> Maybe TakeOffLanding90
  -> Html ()
htmlTakeOffLanding90 _ r =
  div_ [class_ "flighttimecurrencyreport"] $
    do  a_ [id_ "RPT_FlightTimeCurrency"] ""
        a_ [href_ (Text.pack "#RPT_FlightTimeCurrency")] . span_ [class_ "entrytag"] $ "RPT"
        h3_ [class_ "flighttimecurrencyreportname"] "Flight Time Currency Report"
        case r of
          Nothing ->
            span_ [class_ "flighttimenocurrency"] "NIL three take-offs and landings"
          Just x ->
            do  ul_ [] $
                  do  li_ [] $
                        do  span_ [class_ "key"] "Three most recent take-offs"
                            ol_ [] $
                              do  li_ [] $
                                    span_ [class_ "value"] (htmlFlightPointDay (x ^. takeoff1))
                                  li_ [] $
                                    span_ [class_ "value"] (htmlFlightPointDay (x ^. takeoff2))
                                  li_ [] $
                                    span_ [class_ "value"] (htmlFlightPointDay (x ^. takeoff3))
                      li_ [] $
                        do  span_ [class_ "key"] "Three most recent landings"
                            ol_ [] $
                              do  li_ [] $
                                    span_ [class_ "value"] (htmlFlightPointDay (x ^. landing1))
                                  li_ [] $
                                    span_ [class_ "value"] (htmlFlightPointDay (x ^. landing2))
                                  li_ [] $
                                    span_ [class_ "value"] (htmlFlightPointDay (x ^. landing3))
                      li_ [] $
                        do  span_ [class_ "key"] "90-day currency: "
                            span_ [class_ "value"] . fromString . show $ x ^. currency90

takeofflanding ::
  Entry a b c d
  -> ([FlightPoint], [FlightPoint])
takeofflanding (AircraftFlightEntry fl _) =
  let p = fl ^. flightpath
      i = p ^. flightIntermediate
  in  (p ^. flightStart : i, i ++ [p ^. flightEnd])
takeofflanding _ =
  ([], [])

takeoffslandings90 ::
  Logbook a b c d
  -> Maybe TakeOffLanding90
takeoffslandings90 b =
  let (t, l) = foldr
                (\a (t', l') -> let (u', m') = takeofflanding a
                              in  (t' ++ u', m' ++ l'))
                ([], []) (b ^. logbookentries . _Wrapped)
      revsort = sortBy (flip (comparing (^. landingTime)))
      (u, m) = (revsort t, revsort l)
  in  case u of
        t1:t2:t3:_ ->
          case m of
            l1:l2:l3:_ ->
              let tt = t3 ^. landingTime . daytime
                  lt = l3 ^. landingTime . daytime
              in  Just (TakeOffLanding90 t1 t2 t3 l1 l2 l3 (addDays 90 (tt `min` lt)))
            _ ->
              Nothing
        _ ->
          Nothing

htmlSimulatorTimeReport ::
  Logbook a b c d
  -> SimulatorTimeReport
  -> Html ()
htmlSimulatorTimeReport _ r =
  div_ [class_ "simulatortimereport"] $
    do  a_ [id_ "RPT_SimulatorTimeSummary"] ""
        a_ [href_ (Text.pack "#RPT_SimulatorTimeSummary")] . span_ [class_ "entrytag"] $ "RPT"
        h3_ [class_ "simulatortimereportname"] "Simulator Time Summary Report"
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Total Simulator Hours: "
                    span_ [class_ "value"] . htmlTimeAmount $ r ^. hoursInstrumentSimulator
              li_ [] $
                do  span_ [class_ "key"] "Instrument Simulator Hours: "
                    span_ [class_ "value"] . htmlTimeAmount $ r ^. hoursTotalSimulator

htmlFlightTimeReport ::
  Logbook a b c d
  -> FlightTimeReport
  -> Html ()
htmlFlightTimeReport _ r =
  div_ [class_ "flighttimereport"] $
    do  a_ [id_ "RPT_FlightTimeSummary"] ""
        a_ [href_ (Text.pack "#RPT_FlightTimeSummary")] . span_ [class_ "entrytag"] $ "RPT"
        h3_ [class_ "flighttimereportname"] "Flight Time Summary Report"
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Total Flights: "
                    span_ [class_ "value"] . fromString . show $ (r ^. flightsTotal)
              li_ [] $
                do  span_ [class_ "key"] "Total Flight Hours: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursTotal
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursTotalICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursTotalDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursTotalInCommand
                          li_ [] $
                            do  span_ [class_ "key"] "providing instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursInstructing
              li_ [] $
                do  span_ [class_ "key"] "Hours in type: "
                    div_ [class_ "value"] .
                      ul_ [] . Map.foldrWithKey (\y (tl, iu, dl, ic) x ->
                        do  li_ [] $
                              do  span_ [class_ "aircrafttype"] $ fromString y
                                  ul_ [] $
                                    do  li_ [] $
                                          do  span_ [class_ "key"] "total: "
                                              span_ [class_ "value"] . htmlTimeAmount $ tl
                                        li_ [] $
                                          do  span_ [class_ "key"] "in-command under-instruction: "
                                              span_ [class_ "value"] . htmlTimeAmount $ iu
                                        li_ [] $
                                          do  span_ [class_ "key"] "dual under-instruction: "
                                              span_ [class_ "value"] . htmlTimeAmount $ dl
                                        li_ [] $
                                          do  span_ [class_ "key"] "in-command: "
                                              span_ [class_ "value"] . htmlTimeAmount $ ic
                            x) mempty $ r ^. hoursInAircraftType
              li_ [] $
                do  span_ [class_ "key"] "Hours in type variant: "
                    div_ [class_ "value"] .
                      ul_ [] . Map.foldrWithKey (\(y, v) (tl, iu, dl, ic) x ->
                        do  li_ [] $
                              do  span_ [class_ "aircrafttypevariant"] $ fromString (y ++ " " ++ v)
                                  ul_ [] $
                                    do  li_ [] $
                                          do  span_ [class_ "key"] "total: "
                                              span_ [class_ "value"] . htmlTimeAmount $ tl
                                        li_ [] $
                                          do  span_ [class_ "key"] "in-command under-instruction: "
                                              span_ [class_ "value"] . htmlTimeAmount $ iu
                                        li_ [] $
                                          do  span_ [class_ "key"] "dual under-instruction: "
                                              span_ [class_ "value"] . htmlTimeAmount $ dl
                                        li_ [] $
                                          do  span_ [class_ "key"] "in-command: "
                                              span_ [class_ "value"] . htmlTimeAmount $ ic
                            x) mempty $ r ^. hoursInAircraftTypeVariant
              li_ [] $
                do  span_ [class_ "key"] "Hours in registration: "
                    div_ [class_ "value"] .
                      ul_ [] . Map.foldrWithKey (\y (tl, iu, dl, ic) x ->
                        do  li_ [] $
                              do  span_ [class_ "aircraftregistration"] $ fromString y
                                  ul_ [] $
                                    do  li_ [] $
                                          do  span_ [class_ "key"] "total: "
                                              span_ [class_ "value"] . htmlTimeAmount $ tl
                                        li_ [] $
                                          do  span_ [class_ "key"] "in-command under-instruction: "
                                              span_ [class_ "value"] . htmlTimeAmount $ iu
                                        li_ [] $
                                          do  span_ [class_ "key"] "dual under-instruction: "
                                              span_ [class_ "value"] . htmlTimeAmount $ dl
                                        li_ [] $
                                          do  span_ [class_ "key"] "in-command: "
                                              span_ [class_ "value"] . htmlTimeAmount $ ic
                            x) mempty $ r ^. hoursInAircraftRegistration
              li_ [] $
                do  span_ [class_ "key"] "Hours in Single-Engine: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursSingleEngine
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursSingleEngineICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursSingleEngineDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursSingleEngineInCommand
              li_ [] $
                do  span_ [class_ "key"] "Hours in Multi-Engine: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursMultiEngine
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursMultiEngineICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursMultiEngineDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursMultiEngineInCommand
              li_ [] $
                do  span_ [class_ "key"] "Hours in Day: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursDay
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursDayICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursDayDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursDayInCommand
              li_ [] $
                do  span_ [class_ "key"] "Hours in Night: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursNight
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "in-command under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursNightICUS
                          li_ [] $
                            do  span_ [class_ "key"] "dual under-instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursNightDual
                          li_ [] $
                            do  span_ [class_ "key"] "in-command: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursNightInCommand
              li_ [] $
                do  span_ [class_ "key"] "providing instruction: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursInstructing
                    ul_ [] $
                      do  li_ [] $
                            do  span_ [class_ "key"] "providing RA instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursRAInstructing
                                ul_ [] $
                                  do  li_ [] $
                                        do  span_ [class_ "key"] "providing RA Junior instruction: "
                                            span_ [class_ "value"] .
                                              htmlTimeAmount $ r ^. hoursRAJuniorInstructing
                                      li_ [] $
                                        do  span_ [class_ "key"] "providing RA Senior instruction: "
                                            span_ [class_ "value"] .
                                              htmlTimeAmount $ r ^. hoursRASeniorInstructing
                          li_ [] $
                            do  span_ [class_ "key"] "providing GA instruction: "
                                span_ [class_ "value"] .
                                  htmlTimeAmount $ r ^. hoursGAInstructing
                                ul_ [] $
                                  do  li_ [] $
                                        do  span_ [class_ "key"] "providing GA 1 instruction: "
                                            span_ [class_ "value"] .
                                              htmlTimeAmount $ r ^. hoursGA1Instructing
                                      li_ [] $
                                        do  span_ [class_ "key"] "providing GA 2 instruction: "
                                            span_ [class_ "value"] .
                                              htmlTimeAmount $ r ^. hoursGA2Instructing
                                      li_ [] $
                                        do  span_ [class_ "key"] "providing GA 3 instruction: "
                                            span_ [class_ "value"] .
                                              htmlTimeAmount $ r ^. hoursGA3Instructing
              li_ [] $
                do  span_ [class_ "key"] "Hours with PiC: "
                    div_ [class_ "value"] .
                      ul_ [] . Map.foldrWithKey (\a t x ->
                        do  li_ [] $
                              do  span_ [class_ "key"] $
                                    do  htmlAviatorShort a
                                        ": "
                                  span_ [class_ "value"] . htmlTimeAmount $ t
                            x) mempty $ r ^. hoursWithPiC
              li_ [] $
                do  span_ [class_ "key"] "Hours instrument in-flight: "
                    span_ [class_ "value"] .
                      htmlTimeAmount $ r ^. hoursInstrument
