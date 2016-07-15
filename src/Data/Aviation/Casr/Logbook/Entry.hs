{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casr.Logbook.Entry(
  Entry(..)
) where

import Control.Lens(makeClassyPrisms)
import Data.Aviation.Casr.Logbook.AircraftFlight(AircraftFlight)
import Data.Aviation.Casr.Logbook.SimulatorFlight(SimulatorFlight)
import Data.Aviation.Casr.Logbook.Exam(Exam)
import Data.Aviation.Casr.Logbook.Briefing(Briefing)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data Entry ae se ee be =
  AircraftFlightEntry AircraftFlight ae
  | SimulatorFlightEntry SimulatorFlight se
  | ExamEntry Exam ee
  | BriefingEntry Briefing be
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Entry
