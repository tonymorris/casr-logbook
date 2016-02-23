module Data.Aviation.Casr.Logbook.VideoType (
  VideoType(..)  
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown

data VideoType =
  YouTube
  | Vimeo
  deriving (Eq, Ord, Show)

instance Markdown VideoType where
  markdown YouTube =
    "youtube"
  markdown Vimeo =
    "vimeo"
