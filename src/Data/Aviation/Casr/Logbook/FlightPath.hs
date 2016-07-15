{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.FlightPath(
  FlightPath(..)
, directflightpath
, directcircuit
, pointsatdate
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.FlightPoint(FlightPoint, pointatdate)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Ord(Ord)
import Data.String(String)
import Data.Time(Day)
import Prelude(Show)

data FlightPath =
  FlightPath {
    _flightStart :: FlightPoint
  , _flightIntermediate :: [FlightPoint]
  , _flightEnd :: FlightPoint
  } deriving (Eq, Ord, Show)

makeClassy ''FlightPath

directflightpath ::
  FlightPoint
  -> FlightPoint
  -> FlightPath
directflightpath x y =
  FlightPath x [] y

directcircuit ::
  FlightPoint
  -> FlightPath
directcircuit x =
  directflightpath x x

pointsatdate ::
  String
  -> [String]
  -> String
  -> Day
  -> FlightPath
pointsatdate x i y d =
  FlightPath
    (pointatdate x d)
    ((\s -> pointatdate s d) <$> i)
    (pointatdate y d)
