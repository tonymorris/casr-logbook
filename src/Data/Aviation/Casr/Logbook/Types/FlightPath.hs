{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.FlightPath(
  FlightPath(..)
, HasFlightPath(..)
, directflightpath
, directcircuit
, pointsatdate
, flightPathList
, circuitsatdate
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Types.FlightPoint(FlightPoint, pointatdate)
import Data.Eq(Eq)
import Data.Functor((<$>))
import Data.Int(Int)
import Data.List((++), replicate)
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

flightPathList ::
  FlightPath
  -> [FlightPoint]
flightPathList (FlightPath s x e) =
  s : x ++ [e]

circuitsatdate ::
  String
  -> Int
  -> Day
  -> FlightPath
circuitsatdate x n d =
  FlightPath
    (pointatdate x d)
    (replicate n (pointatdate x d))
    (pointatdate x d)
