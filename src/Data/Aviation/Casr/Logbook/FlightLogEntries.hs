module Data.Aviation.Casr.Logbook.FlightLogEntries (
  FlightLogEntries(..)
) where

import Data.Aviation.Casr.Logbook.FlightLogEntry
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.List

newtype FlightLogEntries =
  FlightLogEntries
    [FlightLogEntry]
  deriving (Eq, Ord, Show)

instance Monoid FlightLogEntries where
  mempty =
    FlightLogEntries []
  FlightLogEntries e1 `mappend` FlightLogEntries e2 =
    FlightLogEntries (e1 `mappend` e2)

instance Markdown FlightLogEntries where
  markdown (FlightLogEntries g) =
    intercalate "\n\n----\n\n" (fmap markdown g)
