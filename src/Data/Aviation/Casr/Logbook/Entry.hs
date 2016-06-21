module Data.Aviation.Casr.Logbook.Entry (
  Entry(..)
, flight
, exam
) where

import Data.Aviation.Casr.Logbook.Aircraft
import Data.Aviation.Casr.Logbook.ARN
import Data.Aviation.Casr.Logbook.Date
import Data.Aviation.Casr.Logbook.DayNight
import Data.Aviation.Casr.Logbook.ExamEntry
import Data.Aviation.Casr.Logbook.FlightEntry
import Data.Aviation.Casr.Logbook.FlightPath
import Data.Aviation.Casr.Logbook.Hours
import Data.Aviation.Casr.Logbook.Images
import Data.Aviation.Casr.Logbook.PiC
import Data.Aviation.Casr.Logbook.PoB
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.Aviation.Casr.Logbook.Sequence
import Data.Aviation.Casr.Logbook.TrackLogs
import Data.Aviation.Casr.Logbook.Videos
import Data.Aviation.Casr.Logbook.Visualisations

data Entry =
  EntryExam ExamEntry
  | EntryFlight FlightEntry
  deriving (Eq, Ord, Show)

instance Markdown Entry where
  markdown (EntryExam e) =
    markdown e
  markdown (EntryFlight f) =
    markdown f

instance Html Entry where
  html (EntryExam e) =
    html e
  html (EntryFlight f) =
    html f

flight ::
  Sequence
  -> Date
  -> Aircraft
  -> Hours
  -> PoB
  -> FlightPath
  -> DayNight
  -> PiC
  -> TrackLogs
  -> Visualisations
  -> Images
  -> Videos
  -> Entry
flight sequ date aircraft hours pob flightpath daynight pic tracklogs visualisations images videos =
  EntryFlight (FlightEntry sequ date aircraft hours pob flightpath daynight pic tracklogs visualisations images videos)

exam ::
  Date
  -> String
  -> String
  -> String
  -> ARN
  -> Int
  -> Int
  -> Entry
exam d n dn dr da r m =
  EntryExam (ExamEntry d n dn dr da r m)
