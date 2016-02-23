module Data.Aviation.Casr.Logbook.Sequence (
  Sequence(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.String

newtype Sequence =
  Sequence
    String
  deriving (Eq, Ord, Show)

instance IsString Sequence where
  fromString =
    Sequence

instance Markdown Sequence where
  markdown (Sequence s) =
    "* Sequence: **" ++ s ++ "**\n"
