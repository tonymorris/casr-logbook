module Data.Aviation.Casr.Logbook.FlightPath (
  FlightPath(..)
, pathlist
, directPath
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.List

data FlightPath =
  FlightPath
    String -- start
    [String] -- touch-downs
    String -- end
  deriving (Eq, Ord, Show)

directPath ::
  String
  -> String
  -> FlightPath
directPath fr to =
  FlightPath
    fr
    []
    to

pathlist ::
  FlightPath
  -> [String]
pathlist (FlightPath s x e) =
  s : (x ++ [e])

instance Markdown FlightPath where
  markdown p =
    concat
      [
        "* Flight path: **"
      , intercalate " - " (map markdown (pathlist p))
      , "**\n"
      ]

instance Html FlightPath where
  html p =
    concat
      [
        "<span class=\"heading flightpathheading\">"
      , "Flight path"
      , "</span>"
      , ": "
      , "<span class=\"info flightpathinfo\">"
      , intercalate " &mdash; " (map html (pathlist p))
      , "</span>"
      ]
