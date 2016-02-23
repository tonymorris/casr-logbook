module Data.Aviation.Casr.Logbook.TrackLogType (
  TrackLogType(..)
) where

import Data.Aviation.Casr.Logbook.ImageType
import Data.Aviation.Casr.Logbook.Printer.Markdown

data TrackLogType =
  Gpx
  | Kml
  | Kmz
  | ImageLog ImageType
  deriving (Eq, Ord, Show)
    
instance Markdown TrackLogType where
  markdown Gpx =
    "gpx"
  markdown Kml =
    "kml"
  markdown Kmz =
    "kmz"
  markdown (ImageLog i) =
    markdown i
