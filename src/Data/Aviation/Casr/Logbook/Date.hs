module Data.Aviation.Casr.Logbook.Date (
  Date(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.String
    
newtype Date =
  Date
    String
  deriving (Eq, Ord, Show)

instance IsString Date where
  fromString =
    Date
    
instance Markdown Date where
  markdown (Date s) =
    "* Date: **`" ++ s ++ "`**\n"

instance Html Date where
  html (Date s) =
    concat
      [
        "<span class=\"heading dateheading\">"
      , "Date"
      , "</span>"
      , ": "
      , "<span class=\"info dateinfo\">"
      , html s
      , "</span>"
      ]
