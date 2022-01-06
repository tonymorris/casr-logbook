{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Casr.Logbook.Html.Html(
  htmlTimeAmount
, strTimeAmount
, strEngine
, htmlAircraft
, htmlRatingDay
, htmlRating
, htmlRatingShort
, htmlRatings
, htmlRatingsShort
, htmlAviatorName
, htmlAviatorARN
, htmlAviatorDob
, htmlAviatorRatings
, htmlAviator
, htmlAviatorShort
, htmlFlightPoint
, htmlFlightPath
, htmlCommand
, htmlTimeAmountZero
, htmlTimeAmountZeroWith
, htmlAviators
, htmlAircraftFlightName
, htmlAircraftFlight
, htmlTimeOfDayTime
, htmlTime
, htmlFlightPathTime
, htmlSimulatorFlightName
, htmlSimulatorFlight
, htmlLocation
, htmlExamResult
, htmlExamName
, htmlExam
, htmlBriefingName
, htmlBriefing
, space2dot
, htmlEntryTag
, htmlEntry
, htmlEntries
, htmlLogbook
, htmlTitleAviator
, htmlLogbookDocument
, htmlLogbookHeader
) where

import Control.Applicative((*>))
import Control.Category((.), id)
import Control.Lens
import Control.Monad(when, (=<<), (>>=))
import Data.Aviation.Casr.Logbook.Types
    ( Location(Location),
      Engine(..),
      Time(Time),
      TimeAmount(TimeAmount),
      HasTime(daytime),
      zerotimeamount,
      Rating(Rating),
      Aircraft(Aircraft),
      FlightPoint(FlightPoint),
      Aviator(Aviator),
      HasAircraft(aircraftRegistration),
      DayNight(DayNight),
      HasFlightPoint(point, landingTime),
      HasAviator(arn, surname, firstname),
      FlightPath,
      HasFlightPath(flightEnd, flightStart),
      flightPathList,
      Command(..),
      SimulatorFlight(SimulatorFlight),
      Briefing(Briefing),
      Exam(Exam),
      HasExam(examTime, examName),
      HasSimulatorFlight(simulatortype, simulatorflightname),
      HasBriefing(briefingTime, briefingName),
      AircraftFlight(AircraftFlight),
      HasAircraftFlight(flightpath, flightaircraft, aircraftflightname),
      Entry(..),
      Entries(..),
      Logbook(Logbook),
      HasLogbook(logbookaviator),
      Instruction,
      HasInstructionRating(instructionRating),
      HasInstructionLesson(lesson, student, instructionLesson),
      shortStringRating )
import Data.Bool(not)
import Data.Char(toUpper)
import Data.Digit(DecDigit, charDecimal)
import Data.Eq((==))
import Data.Foldable(fold, sequence_, mapM_, null)
import Data.Function(($))
import Data.Functor((<$>))
import Data.Int(Int)
import Data.List(intersperse, concat)
import Data.Maybe(Maybe, maybe)
import Data.Monoid(Monoid, (<>), mempty)
import Data.String(String, fromString)
import Data.Time(Day, TimeOfDay)
import Data.Text(Text)
import qualified Data.Text as Text(pack)
import Lucid(
    id_
  , class_
  , h1_
  , h2_
  , h3_
  , span_
  , a_
  , div_
  , href_
  , src_
  , type_
  , script_
  , body_
  , title_
  , rel_
  , link_
  , title_
  , html_
  , doctype_
  , head_
  , lang_
  , hr_
  , ul_
  , li_
  , Html
  , toHtmlRaw
  )
import Text.Printf(printf)

import Prelude(show, fromIntegral, (/), (*), Double)

htmlTimeAmount ::
  TimeAmount
  -> Html ()
htmlTimeAmount t =
  span_ [] $
    do  fromString (strTimeAmount t)
        "hrs"

strTimeAmount ::
  TimeAmount
  -> String
strTimeAmount (TimeAmount h x) =
  show h <> "." <> [charDecimal # x]

strEngine ::
  Engine
  -> String
strEngine Single =
  "single-engine"
strEngine Multi =
  "multi-engine"

htmlAircraft ::
  AircraftFlight
  -> Aircraft
  -> Html ()
htmlAircraft _ (Aircraft t r e) =
  span_ [class_ "aircraft"] $
    do  span_ [class_ "aircrafttype"] (fromString t)
        " "
        span_ [class_ "aircraftregistration"] (fromString r)
        " "
        span_ [class_ "aircraftengine"] (fromString (strEngine e))

htmlRatingDay ::
  Maybe Day
  -> Html ()
htmlRatingDay =
  maybe mempty (\q ->
    do  " "
        span_ [] $
          fromString (show q))

htmlRating ::
  Rating
  -> Html ()
htmlRating (Rating n d) =
  span_ [] $
    do  span_ [] (fromString n)
        htmlRatingDay d

htmlRatingShort ::
  Rating
  -> Html ()
htmlRatingShort (Rating n _) =
  span_ [] (fromString n)

htmlRatings ::
  [Rating]
  -> Html ()
htmlRatings =
  sequence_ . intersperse ", " . (htmlRating <$>)

htmlRatingsShort ::
  [Rating]
  -> Html ()
htmlRatingsShort =
  sequence_ . intersperse ", " . (htmlRatingShort <$>)

htmlAviatorName ::
  String
  -> String
  -> Html ()
htmlAviatorName s f =
  do  li_ [id_ "aviatorname"] $
        do  span_ [class_ "key"] "Name: "
            span_ [class_ "value"] $
              do  fromString (toUpper <$> s)
                  when (not . null $ f) $
                    toHtmlRaw (", " <> f)

htmlAviatorARN ::
  [DecDigit]
  -> Html ()
htmlAviatorARN a =
  when (not . null $ a) $
    do  li_ [id_ "aviatorarn"] $
          do  span_ [class_ "key"] "ARN: "
              span_ [class_ "value"] $
                fromString (a >>= (\d -> [charDecimal # d]))

htmlAviatorDob ::
  Maybe Day
  -> Html ()
htmlAviatorDob =
  maybe mempty (\q ->
    do  li_ [id_ "aviatordob"] $
          do  span_ [class_ "key"] "Date of Birth: "
              span_ [class_ "value"] .
                fromString . show $ q)

htmlAviatorRatings ::
  [Rating]
  -> Html ()
htmlAviatorRatings r =
  when (not . null $ r) $
    do  li_ [id_ "aviatorratings"] $
          do  span_ [class_ "key"] "Ratings: "
              span_ [class_ "value"] .
                htmlRatings $ r

htmlAviator ::
  Aviator
  -> Html ()
htmlAviator (Aviator s f a d r) =
  div_ [id_ "aviator", class_ "aviator"] .
    ul_ [] $
      do  htmlAviatorName s f
          htmlAviatorARN a
          htmlAviatorDob d
          htmlAviatorRatings r

htmlAviatorShort ::
  Aviator
  -> Html ()
htmlAviatorShort (Aviator s f a _ r) =
  do  fromString f
      " "
      fromString s
      when (not . null $ a) " "
      fromString (a >>= (\d -> [charDecimal # d]))
      when (not . null $ r) " "
      htmlRatingsShort r

htmlFlightPoint ::
  AircraftFlight
  -> FlightPoint
  -> Html ()
htmlFlightPoint _ (FlightPoint p _ _) =
  span_ [class_ "flightpoint"] $
    fromString p

htmlFlightPath ::
  AircraftFlight
  -> FlightPath
  -> Html ()
htmlFlightPath fl p =
  span_ [class_ "flightpath"] $
    fold (intersperse (toHtmlRaw (" &mdash; " :: Text)) (htmlFlightPoint fl <$> flightPathList p))

htmlInstruction ::
  Instruction
  -> Html ()
htmlInstruction i =
  let r = view instructionRating i
      l = view instructionLesson i
      a = view student l
  in  do  span_ [class_ "instructionrating"] (toHtmlRaw (shortStringRating r))
          span_ [class_ "commandphrase"] " for "
          span_ [class_ "commandaviator"] $ htmlAviatorShort a
          maybe mempty (\c ->
              do  span_ [class_ "commandphrase"] " "
                  span_ [class_ "instructionlesson"] (toHtmlRaw c)
            ) (view lesson l)

htmlCommand ::
  AircraftFlight
  -> Command
  -> Html ()
htmlCommand _ InCommand =
  span_ [class_ "command incommand"] "In-Command"
htmlCommand _ (InCommandInstructing a) =
  do  span_ [class_ "command incommandinstruction"] "Instruction"
      span_ [class_ "commandphrase"] " as "
      span_ [class_ "commandinstruction"] $ htmlInstruction a
htmlCommand _ (ICUS a) =
  do  span_ [class_ "command incommandunderinstruction"] "In-Command Under-Instruction"
      span_ [class_ "commandphrase"] " by "
      span_ [class_ "commandaviator"] $ htmlAviatorShort a
htmlCommand _ (Dual a) =
  do  span_ [class_ "command dualunderinstruction"] "Dual Under-Instruction"
      span_ [class_ "commandphrase"] " by "
      span_ [class_ "commandaviator"] $ htmlAviatorShort a
htmlCommand _ (ApprovedSolo a) =
  do  span_ [class_ "command approvedsolo"] "Approved Solo"
      span_ [class_ "commandphrase"] " by "
      span_ [class_ "commandaviator"] $ htmlAviatorShort a

htmlTimeAmountZero ::
  TimeAmount
  -> Html ()
htmlTimeAmountZero =
  htmlTimeAmountZeroWith id

htmlTimeAmountZeroWith ::
  Monoid a =>
  (Html () -> a)
  -> TimeAmount
  -> a
htmlTimeAmountZeroWith f z =
  if z == zerotimeamount
    then
      mempty
    else
      f (htmlTimeAmount z)

htmlAviators ::
  [Aviator]
  -> Html ()
htmlAviators =
  ul_ [] .
    mapM_ (li_ [] . htmlAviatorShort)

htmlAircraftFlightName ::
  String
  -> Html ()
htmlAircraftFlightName n =
  h3_ [class_ "aircraftflightname"] $
          fromString n

htmlAircraftFlight ::
  AircraftFlight
  -> Html ()
htmlAircraftFlight fl@(AircraftFlight n a c (DayNight d m) p o i) =
  div_ [class_ "aircraftflight"] $
    do  htmlAircraftFlightName n
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Time: "
                    span_ [class_ "value"] .
                     htmlFlightPathTime $ p
              li_ [] $
                do  span_ [class_ "key"] "Aircraft: "
                    span_ [class_ "value"] .
                     htmlAircraft fl $ a
              li_ [] $
                do  span_ [class_ "key"] "Command: "
                    span_ [class_ "value"] .
                     htmlCommand fl $ c
              htmlTimeAmountZeroWith (\t ->
                li_ [] $
                do  span_ [class_ "key"] "Amount (day): "
                    span_ [class_ "value"] t) d
              htmlTimeAmountZeroWith (\t ->
                li_ [] $
                do  span_ [class_ "key"] "Amount (night): "
                    span_ [class_ "value"] t) m
              htmlTimeAmountZeroWith (\t ->
                li_ [] $
                do  span_ [class_ "key"] "Amount (instrument): "
                    span_ [class_ "value"] t) i
              li_ [] $
                do  span_ [class_ "key"] "Flight Path: "
                    span_ [class_ "value"] .
                      htmlFlightPath fl $ p
              when (not . null $ o) . li_ [] $
                do  span_ [class_ "key"] "Other Crew: "
                    span_ [class_ "value"] .
                      htmlAviators $ o

htmlTimeOfDayTime ::
  Maybe TimeOfDay
  -> Html ()
htmlTimeOfDayTime =
  maybe mempty (\e -> do  " "
                          fromString (show e))

htmlTime ::
  Time
  -> Html ()
htmlTime (Time t d) =
  span_ [class_ "time"] $
    do  fromString (show t)
        htmlTimeOfDayTime d

htmlFlightPathTime ::
  FlightPath
  -> Html()
htmlFlightPathTime p =
  let s = p ^. flightStart . landingTime
      e = p ^. flightEnd . landingTime
  in  if s == e
        then
          htmlTime s
        else
          do  htmlTime s
              toHtmlRaw (" &mdash; " :: Text)
              htmlTime e

htmlSimulatorFlightName ::
  String
  -> Html ()
htmlSimulatorFlightName n =
  h3_ [class_ "simulatorflightname"] $
          fromString n

htmlSimulatorFlight ::
  SimulatorFlight
  -> Html ()
htmlSimulatorFlight (SimulatorFlight n t y o a i) =
  div_ [class_ "simulatorflight"] $
    do  htmlSimulatorFlightName n
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Time: "
                    span_ [class_ "value"] .
                     htmlTime $ t
              li_ [] $
                do  span_ [class_ "key"] "Type: "
                    span_ [class_ "value"] (fromString y)
              when (not . null $ o) . li_ [] $
                do  span_ [class_ "key"] "Other Crew: "
                    div_ [class_ "value"] .
                      htmlAviators $ o
              li_ [] $
                do  span_ [class_ "key"] "Amount: "
                    span_ [class_ "value"] .
                     htmlTimeAmount $ a
              li_ [] $
                do  span_ [class_ "key"] "Instrument: "
                    span_ [class_ "value"] .
                     htmlTimeAmount $ i

htmlLocation ::
  Location
  -> Html ()
htmlLocation (Location n t o) =
  span_ [class_ "location"] $
    do  fromString n
        " "
        let t' = fromString (show t)
            o' = fromString (show o)
        span_ [class_ "locationopenstreetmap"] $
          a_ [href_ ("http://www.openstreetmap.org/?mlat=" <> t' <> "&mlon=" <> o' <> "#map=16/" <> t' <> "/" <> o')]
          "osm"
        " "
        span_ [class_ "locationgooglemaps"] $
          a_ [href_ ("https://www.google.com/maps/?q=" <> t' <> "," <> o')]
          "gmap"

htmlExamResult ::
  Int
  -> Int
  -> Html ()
htmlExamResult x y =
  do  fromString (show x)
      "/"
      fromString (show y)

htmlExamName ::
  String
  -> Html ()
htmlExamName n =
  h3_ [class_ "examname"] $
    fromString n

htmlExam ::
  Exam
  -> Html ()
htmlExam (Exam n l t a r m) =
  let r' = do  span_ [class_ "examresult"] . fromString . show $ r
               span_ [class_ "examresultoutof"] "/"
               span_ [class_ "examresultmaximum"] . fromString . show $ m
               " ("
               span_ [class_ "examresultpercentage"] . fromString . printf "%.2f" $ (100 * fromIntegral r / fromIntegral m :: Double)
               span_ [class_ "examresultpercentsign"] "%"
               ")"
  in  div_ [class_ "exam"] $
        do  htmlExamName n
            ul_ [] $
              do  li_ [] $
                    do  span_ [class_ "key"] "Time: "
                        span_ [class_ "value"] .
                          htmlTime $ t
                  li_ [] $
                    do  span_ [class_ "key"] "Location: "
                        span_ [class_ "value"] .
                          htmlLocation $ l
                  li_ [] $
                    do  span_ [class_ "key"] "Delegate: "
                        span_ [class_ "value"] .
                          htmlAviatorShort $ a
                  li_ [] $
                    do  span_ [class_ "key"] "Result: "
                        span_ [class_ "value"] r'

htmlBriefingName ::
  String
  -> Html ()
htmlBriefingName n =
  h3_ [class_ "briefingname"] $
    fromString n

htmlBriefing ::
  Briefing
  -> Html ()
htmlBriefing (Briefing n l t a m) =
  div_ [class_ "briefing"] $
    do  htmlBriefingName n
        ul_ [] $
          do  li_ [] $
                do  span_ [class_ "key"] "Time: "
                    span_ [class_ "value"] .
                      htmlTime $ t
              li_ [] $
                do  span_ [class_ "key"] "Location: "
                    span_ [class_ "value"] .
                      htmlLocation $ l
              li_ [] $
                do  span_ [class_ "key"] "Amount: "
                    span_ [class_ "value"] .
                      htmlTimeAmountZero $ m
              li_ [] $
                do  span_ [class_ "key"] "Briefer: "
                    span_ [class_ "value"] .
                      htmlAviatorShort $ a

space2dot ::
  String
  -> String
space2dot =
  (<$>) $ \c -> case c of
                ' ' -> '.'
                _   -> c

htmlEntryTag ::
  Entry a b c d
  -> Html ()
htmlEntryTag (AircraftFlightEntry e _) =
  let lk = space2dot . concat $
                          [
                            "FLT_"
                          , e ^. aircraftflightname
                          , "_"
                          , e ^. flightaircraft . aircraftRegistration
                          , "_"
                          , e ^. flightpath . flightStart . point
                          , "-"
                          , e ^. flightpath . flightEnd . point
                          ]
  in  do  a_ [id_ (Text.pack lk)] ""
          a_ [href_ (Text.pack ('#' : lk))] . span_ [class_ "entrytag"] $ "FLT"

htmlEntryTag (SimulatorFlightEntry e _) =
  let lk = space2dot . concat $
                          [
                            "SIM_"
                          , e ^. simulatorflightname
                          , "_"
                          , e ^. simulatortype
                          ]
  in  do  a_ [id_ (Text.pack lk)] ""
          a_ [href_ (Text.pack ('#' : lk))] . span_ [class_ "entrytag"] $ "SIM"
htmlEntryTag (ExamEntry e _) =
  let lk = space2dot . concat $
                          [
                            "EXM_"
                          , e ^. examName
                          , "_"
                          , show (e ^. examTime . daytime)
                          ]
  in  do  a_ [id_ (Text.pack lk)] ""
          a_ [href_ (Text.pack ('#' : lk))] . span_ [class_ "entrytag"] $ "EXM"
htmlEntryTag (BriefingEntry e _) =
  let lk = space2dot . concat $
                          [
                            "BRF_"
                          , e ^. briefingName
                          , "_"
                          , show (e ^. briefingTime . daytime)
                          ]
  in  do  a_ [id_ (Text.pack lk)] ""
          a_ [href_ (Text.pack ('#' : lk))] . span_ [class_ "entrytag"] $ "BRF"

htmlEntry ::
  (AircraftFlight -> a -> Html x)
  -> (SimulatorFlight -> b -> Html x)
  -> (Exam -> c -> Html x)
  -> (Briefing -> d -> Html x)
  -> Entry a b c d
  -> Html x
htmlEntry aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' x =
  do  htmlEntryTag x
      case x of
        AircraftFlightEntry e ae ->
          do  div_ [] $
                do  htmlAircraftFlight e
                    aircraftFlightMeta' e ae
        SimulatorFlightEntry e ae ->
          do  div_ [] $
                do  htmlSimulatorFlight e
                    simulatorFlightMeta' e ae
        ExamEntry e ae ->
          do  div_ [] $
                do  htmlExam e
                    examMeta' e ae
        BriefingEntry e ae ->
          do  div_ [] $
                do  htmlBriefing e
                    briefingMeta' e ae

htmlEntries ::
  (AircraftFlight -> a -> Html x)
  -> (SimulatorFlight -> b -> Html x)
  -> (Exam -> c -> Html x)
  -> (Briefing -> d -> Html x)
  -> Entries a b c d
  -> Html ()
htmlEntries aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' (Entries es) =
  mapM_ (\e -> hr_ [] *> htmlEntry aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' e) es

htmlLogbook ::
  (AircraftFlight -> a -> Html x)
  -> (SimulatorFlight -> b -> Html x)
  -> (Exam -> c -> Html x)
  -> (Briefing -> d -> Html x)
  -> Html ()
  -> Logbook a b c d
  -> Html ()
htmlLogbook aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' reports' (Logbook a es) =
  do  htmlAviator a
      hr_ []
      reports'
      htmlEntries aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' es

htmlTitleAviator ::
  Aviator
  -> Html ()
htmlTitleAviator a =
  fromString (concat
                [
                  a ^. firstname
                , " "
                , a ^. surname
                , " ("
                , (\d -> [charDecimal # d]) =<< (a ^. arn)
                , ")"
                ])

htmlLogbookDocument ::
  (AircraftFlight -> a -> Html x)
  -> (SimulatorFlight -> b -> Html x)
  -> (Exam -> c -> Html x)
  -> (Briefing -> d -> Html x)
  -> Html ()
  -> Logbook a b c d
  -> Html ()
htmlLogbookDocument aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' reports' b =
  do  doctype_
      html_ [lang_ "en"] $
        do  head_ $
              do  title_ ("Pilot Personal Logbook " <> toHtmlRaw (" &mdash; " :: Text) <> htmlTitleAviator (b ^. logbookaviator))
                  link_ [href_ "https://fonts.googleapis.com/css?family=Inconsolata:400,700", rel_ "stylesheet", type_ "text/css"]
                  link_ [href_ "casr-logbook.css", rel_ "stylesheet", type_ "text/css"]
                  link_ [href_ "/atom.xml", rel_ "alternate", type_ "application/atom+xml", title_ "Atom feed"]
                  script_ [type_ "text/javascript", src_ "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"] ("" :: Text)
                  script_ [type_ "text/javascript", src_ "https://raw.github.com/Mathapedia/LaTeX2HTML5/master/latex2html5.min.js"] ("" :: Text)
            body_ [class_ "casr-logbook"] $
              do  htmlLogbookHeader b
                  htmlLogbook aircraftFlightMeta' simulatorFlightMeta' examMeta' briefingMeta' reports' b

htmlLogbookHeader ::
  Logbook a b c d
  -> Html ()
htmlLogbookHeader _ =
  do  div_ [id_ "header", class_ "header"] $
        h1_ "Pilot Personal Log Book"
      div_ [id_ "subheader", class_ "subheader"] $
        h2_ $
          do  "Civil Aviation Safety Regulation 1998 (61.345)"
              " "
              span_ [class_ "austlii"] $
                a_ [href_ "http://www.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s61.345.html"] "austlii.edu.au"
