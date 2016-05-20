module Data.Aviation.Casr.Logbook.Sequence (
  Sequence(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
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
    "* Sequence: **" ++ markdown s ++ "**\n"

instance Html Sequence where
  html (Sequence s) =
    concat
      [
        "<span class=\"heading sequenceheading\">"
      , "Sequence"
      , "</span>"
      , ": "
      , "<span class=\"info sequenceinfo\">"
      , html s
      , "</span>"
      ]
