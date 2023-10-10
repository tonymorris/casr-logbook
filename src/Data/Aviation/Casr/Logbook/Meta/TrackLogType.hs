{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.TrackLogType(
  TrackLogType(Gpx, Kml, Kmz, ImageTrackLog)
, AsTrackLogType(..)
, gpx
, kml
, kmz
) where

import Control.Lens(makeClassyPrisms, ( # ))
import Data.Aviation.Casr.Logbook.Meta.ImageType ( ImageType )
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data TrackLogType =
  Gpx
  | Kml
  | Kmz
  | ImageTrackLog ImageType
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TrackLogType

gpx ::
  AsTrackLogType t =>
  t
gpx =
  _Gpx # ()

kml ::
  AsTrackLogType t =>
  t
kml =
  _Kml # ()

kmz ::
  AsTrackLogType t =>
  t
kmz =
  _Kmz # ()
