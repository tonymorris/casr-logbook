module Data.Aviation.Casr.Logbook.PiC (
  PiC(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.String

newtype PiC =
  PiC
    String
  deriving (Eq, Ord, Show)

instance IsString PiC where
  fromString =
    PiC
    
instance Markdown PiC where
  markdown (PiC s) =
    "* Pilot in Command: **" ++ markdown s ++ "**\n"

instance Html PiC where
  html (PiC s) =
    concat
      [
        "<span class=\"heading picheading\">"
      , "Pilot in Command"
      , "</span>"
      , ": "
      , "<span class=\"info picinfo\">"
      , html s
      , "</span>"
      ]
    