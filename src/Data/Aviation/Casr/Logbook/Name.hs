module Data.Aviation.Casr.Logbook.Name (
  Name(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.String

newtype Name =
  Name
    String
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString =
    Name

instance Markdown Name where
  markdown (Name s) =
    "### " ++ markdown s ++ "\n"

instance Html Name where
  html (Name s) =
    "<span class=\"heading nameheading\">Name: </span><span class=\"nameinfo\">" ++ html s ++ "</span>"
