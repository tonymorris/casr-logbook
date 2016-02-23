module Data.Aviation.Casr.Logbook.DOB (
  DOB(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
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
    "* Date of Birth: **`" ++ s ++ "`**\n"
