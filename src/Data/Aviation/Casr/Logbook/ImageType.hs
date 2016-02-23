module Data.Aviation.Casr.Logbook.ImageType (
  ImageType(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown

data ImageType =
  Png
  | Jpg
  deriving (Eq, Ord, Show)

instance Markdown ImageType where
  markdown Png =
    "png"
  markdown Jpg =
    "jpg"
