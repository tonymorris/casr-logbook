module Data.Aviation.Casr.Logbook.TrackLogType (
  TrackLogType(..)
) where

import Data.Aviation.Casr.Logbook.ImageType
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html

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

instance Html TrackLogType where
  html Gpx =
    "gpx"
  html Kml =
    "kml"
  html Kmz =
    "kmz"
  html (ImageLog i) =
    html i
