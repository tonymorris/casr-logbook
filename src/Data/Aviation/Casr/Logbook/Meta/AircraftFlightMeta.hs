{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.AircraftFlightMeta(
  AircraftFlightMeta(AircraftFlightMeta)
, HasAircraftFlightMeta(..)
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Meta.AircraftFlightExpense
import Data.Aviation.Casr.Logbook.Meta.Image
import Data.Aviation.Casr.Logbook.Meta.Passenger
import Data.Aviation.Casr.Logbook.Meta.TrackLog
import Data.Aviation.Casr.Logbook.Meta.Video
import Data.Aviation.Casr.Logbook.Meta.Visualisation
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data AircraftFlightMeta =
  AircraftFlightMeta {
    _tracklogs :: [TrackLog]
  , _visualisations :: [Visualisation]
  , _images :: [Image]
  , _videos :: [Video]
  , _expenses :: [AircraftFlightExpense]
  , _pax :: [Passenger]
  } deriving (Eq, Ord, Show)

makeClassy '' AircraftFlightMeta
