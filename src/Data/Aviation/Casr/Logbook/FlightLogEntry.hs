module Data.Aviation.Casr.Logbook.FlightLogEntry (
  FlightLogEntry(..)
) where

import Data.Aviation.Casr.Logbook.Aircraft
import Data.Aviation.Casr.Logbook.Date
import Data.Aviation.Casr.Logbook.DayNight
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

{-

This data structure currently only handles Day VFR. More modifications are
likely to result in a complete flight log entry structure.

-}
data FlightLogEntry =
  FlightLogEntry
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
  markdown (FlightLogEntry sequ date aircraft hours pob flightpath daynight pic tracklogs visualisations images videos) =
    concat
      [
        markdown date
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

instance Html FlightLogEntry where
  html (FlightLogEntry sequ date aircraft hours pob flightpath daynight pic tracklogs visualisations images videos) =
    let onethen x c r = concat $
                 case r of
                   [] ->
                     []
                   _ ->
                     [
                       "<"
                     , x
                     , " class=\""
                     , c
                     , "\">"
                     , r 
                     , "</"
                     , x
                     , ">"
                     ]
        li = onethen "li"
        div' = onethen "div"             
    in  concat
          [
            "<div class=\"flightlogsequence\">"
          , "<h3 class=\"sequence\">"
          , html sequ
          , "</h3>"
          , "</div>"
          , "<ul>"
          , "<li class=\"date\">"
          , html date
          , "</li>"
          , "<li class=\"aircraft\">"
          , html aircraft
          , "</li>"
          , "<li class=\"hours\">"
          , html hours
          , "</li>"
          , "<li class=\"pob\">"
          , html pob
          , "</li>"
          , "<li class=\"flightpath\">"
          , html flightpath
          , "</li>"
          , "<li class=\"daynight\">"
          , html daynight
          , "</li>"
          , "<li class=\"pic\">"
          , html pic
          , "</li>"
          , li "tracklogs" (html tracklogs)
          , li "videos" (html videos) 
          , li "visualisations" (html visualisations) 
          , "</ul>"
          , div' "images" (html images) 
          ]
