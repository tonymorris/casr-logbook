module Data.Aviation.Casr.Logbook.DayNight (
  DayNight(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html

data DayNight =
  Day
  | Night
  | DayNight
  deriving (Eq, Ord, Show)
    
instance Markdown DayNight where
  markdown x =
    concat
      [
        "* Day/Night: **"
      , case x of
          Day -> "Day"
          Night -> "Night"
          DayNight -> "Day & Night"
      , "**\n"
      ]
    
instance Html DayNight where
  html x =
    concat
      [
        "<span class=\"heading daynightheading\">"
      , "Day/Night"
      , "</span>"
      , ": "
      , "<span class=\"info daynightinfo\">"
      , case x of
          Day -> "Day"
          Night -> "Night"
          DayNight -> "Day &amp; Night"
      , "</span>"
      ]
