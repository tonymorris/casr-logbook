{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.AircraftFlightExpense(
  AircraftFlightExpense(ExpenseAircraftUsage, ExpenseAircraftLanding)
, AsAircraftFlightExpense(..)
) where

import Control.Lens(makeClassyPrisms)
import Data.Aviation.Casr.Logbook.Meta.AircraftLandingExpense
import Data.Aviation.Casr.Logbook.Meta.AircraftUsageExpense
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data AircraftFlightExpense =
  ExpenseAircraftUsage AircraftUsageExpense
  | ExpenseAircraftLanding AircraftLandingExpense
  deriving (Eq, Ord, Show)

makeClassyPrisms ''AircraftFlightExpense
