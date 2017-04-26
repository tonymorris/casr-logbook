{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.TrackLogType(
  TrackLogType(Gpx, Kml, Kmz, ImageTrackLog)
, AsTrackLogType(..)
) where

import Control.Lens(makeClassyPrisms)
import Data.Aviation.Casr.Logbook.Meta.ImageType
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
