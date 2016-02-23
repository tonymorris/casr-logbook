module Data.Aviation.Casr.Logbook.FlightLogEntry (
  FlightLogEntry(..)
) where

import Data.Aviation.Casr.Logbook.Aircraft
import Data.Aviation.Casr.Logbook.Date
import Data.Aviation.Casr.Logbook.DayNight
import Data.Aviation.Casr.Logbook.FlightPath
import Data.Aviation.Casr.Logbook.Hours
import Data.Aviation.Casr.Logbook.Images
import Data.Aviation.Casr.Logbook.Name
import Data.Aviation.Casr.Logbook.PiC
import Data.Aviation.Casr.Logbook.PoB
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Sequence
import Data.Aviation.Casr.Logbook.TrackLogs
import Data.Aviation.Casr.Logbook.Videos
import Data.Aviation.Casr.Logbook.Visualisations

{-

This data structure currently only handles Day VFR. More modifications are
likely to result in a complete flight log entry structure.

-}
data FlightLogEntry =
  FlightLogEntry
    Name
    Sequence
    Date 
    Aircraft
    Hours
    PoB
    FlightPath
    DayNight
    PiC
    TrackLogs
    Visualisations
    Images
    Videos
  deriving (Eq, Ord, Show)
    
instance Markdown FlightLogEntry where
  markdown (FlightLogEntry name sequ date aircraft hours pob flightpath daynight pic tracklogs visualisations images videos) =
    concat
      [
        markdown name
      , markdown date
      , markdown sequ
      , markdown aircraft
      , markdown hours
      , markdown pob
      , markdown flightpath
      , markdown daynight
      , markdown pic
      , markdown tracklogs
      , markdown videos
      , markdown visualisations
      , markdown images
      ]
