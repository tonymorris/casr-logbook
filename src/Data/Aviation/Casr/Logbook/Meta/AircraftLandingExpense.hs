{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.AircraftLandingExpense(
  AircraftLandingExpense(AircraftLandingExpense)
, HasAircraftLandingExpense(..)
) where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data AircraftLandingExpense =
  AircraftLandingExpense {
    _aircraftlandingexpenseamount :: Int
  , _aircraftlandingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftLandingExpense
