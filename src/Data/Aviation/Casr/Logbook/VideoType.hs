module Data.Aviation.Casr.Logbook.VideoType (
  VideoType(..)  
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html

data VideoType =
  YouTube
  | Vimeo
  | Bambuser
  deriving (Eq, Ord, Show)

instance Markdown VideoType where
  markdown YouTube =
    "youtube"
  markdown Vimeo =
    "vimeo"
  markdown Bambuser =
    "bambuser"

instance Html VideoType where
  html YouTube =
    "youtube"
  html Vimeo =
    "vimeo"
  html Bambuser =
    "bambuser"
