module Data.Aviation.Casr.Logbook.ImageType (
  ImageType(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html

data ImageType =
  Png
  | Jpg
  deriving (Eq, Ord, Show)

instance Markdown ImageType where
  markdown Png =
    "png"
  markdown Jpg =
    "jpg"

instance Html ImageType where
  html Png =
    "png"
  html Jpg =
    "jpg"
