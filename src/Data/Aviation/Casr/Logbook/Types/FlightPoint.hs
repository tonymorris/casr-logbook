{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.FlightPoint(
  FlightPoint(..)
, HasFlightPoint(..)
, pointatdate
, runwayatdate
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Types.Time(Time, dayonly)
import Data.Eq(Eq)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.String(String)
import Data.Time(Day)
import Prelude(Show)

data FlightPoint =
  FlightPoint {
    _point :: String
  , _runway :: Maybe String
  , _landingTime :: Time
  } deriving (Eq, Ord, Show)

makeClassy ''FlightPoint

pointatdate ::
  String
  -> Day
  -> FlightPoint
pointatdate a d =
  FlightPoint
    a
    Nothing
    (dayonly d)

runwayatdate ::
  String
  -> String
  -> Day
  -> FlightPoint
runwayatdate a r d =
  FlightPoint
    a
    (Just r)
    (dayonly d)
