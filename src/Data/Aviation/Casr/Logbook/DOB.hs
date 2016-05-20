module Data.Aviation.Casr.Logbook.DOB (
  DOB(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.String

newtype DOB =
  DOB
    String
  deriving (Eq, Ord, Show)

instance IsString DOB where
  fromString =
    DOB

instance Markdown DOB where
  markdown (DOB s) =
    "* Date of Birth: **`" ++ markdown s ++ "`**\n"

instance Html DOB where
  html (DOB s) =
    "<span class=\"heading dobheading\">Date of Birth: </span><span class=\"dobinfo\">" ++ html s ++ "</span>"
