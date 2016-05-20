module Data.Aviation.Casr.Logbook.Aircraft (
  Aircraft(..)
) where

import Data.Aviation.Casr.Logbook.Engine
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html

data Aircraft =
  Aircraft
    String -- type
    String -- registration
    Engine
  deriving (Eq, Ord, Show)

instance Markdown Aircraft where
  markdown (Aircraft t r e) =
    concat
      [
        "* Aircraft"
      , "\n  * Type: **"
      , markdown t
      , "**\n  * Registration: **`"
      , markdown r
      , "`**\n  * Engine: **`"
      , markdown e
      , "`**\n"
      ]

instance Html Aircraft where
  html (Aircraft t r e) =
    concat
      [
        "<span class=\"heading aircraftheading\">"
      , "Aircraft"
      , "</span>"
      , ": "
      , "<div class=\"info aircraftinfo\">"
      , "<ul>"
      , "<li>"
      , "<span class=\"heading aircrafttypeheading\">"
      , "Type"
      , "</span>"
      , ": "
      , "<span class=\"info aircrafttypeinfo\">"
      , html t
      , "</span>"
      , "</li>"
      , "<li>"
      , "<span class=\"heading aircraftregistrationheading\">"
      , "Registration"
      , "</span>"
      , ": "
      , "<span class=\"info aircraftregistrationinfo\">"
      , html r
      , "</span>"
      , "</li>"
      , "<li>"
      , "<span class=\"heading aircraftengineheading\">"
      , "Engine"
      , "</span>"
      , ": "
      , "<span class=\"info aircraftengineinfo\">"
      , html e
      , "</span>"
      , "</li>"
      , "</ul>"
      , "</div>"
      ]
