module Data.Aviation.Casr.Logbook.DayNight (
  DayNight(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown

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
