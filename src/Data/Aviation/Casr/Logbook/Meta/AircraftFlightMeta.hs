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
import Data.Monoid
import Data.Semigroup

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

instance Semigroup AircraftFlightMeta where
  AircraftFlightMeta t1 s1 i1 v1 e1 p1 <> AircraftFlightMeta t2 s2 i2 v2 e2 p2 =
    AircraftFlightMeta (t1 <> t2) (s1 <> s2) (i1 <> i2) (v1 <> v2) (e1 <> e2) (p1 <> p2)

instance Monoid AircraftFlightMeta where
  mempty =
    AircraftFlightMeta mempty mempty mempty mempty mempty mempty
