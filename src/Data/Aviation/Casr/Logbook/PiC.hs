module Data.Aviation.Casr.Logbook.PiC (
  PiC(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
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
    "* Pilot in Command: **" ++ s ++ "**\n"
