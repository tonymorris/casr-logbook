module Data.Aviation.Casr.Logbook.FlightLog (
  FlightLog(..)
) where

import Data.Aviation.Casr.Logbook.ARN
import Data.Aviation.Casr.Logbook.DOB
import Data.Aviation.Casr.Logbook.FlightLogEntries
import Data.Aviation.Casr.Logbook.Name
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Totals

data FlightLog =
  FlightLog
    Name
    DOB
    ARN
    FlightLogEntries
  deriving (Eq, Ord, Show)
    
instance Markdown FlightLog where
  markdown (FlightLog (Name name) dob arn entries) =
    concat
      [
        "# Pilot Personal Log Book\n"
      , "### Civil Aviation Safety Regulation 1998 (61.345) [*austlii.edu.au*](http://www.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s61.345.html)\n\n"
      , "* "
      , name
      , "\n"
      , markdown dob
      , markdown arn
      , "\n----\n\n"
      , markdown (totals entries)
      , "\n----\n\n"
      , markdown entries
      ]
