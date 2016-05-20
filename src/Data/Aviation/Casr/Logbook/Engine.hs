module Data.Aviation.Casr.Logbook.Engine (
  Engine(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html

data Engine =
  Single
  | Multi
  deriving (Eq, Ord, Show)
    
instance Markdown Engine where
  markdown Single =
    "single"
  markdown Multi =
    "multi"

instance Html Engine where
  html Single =
    "single"
  html Multi =
    "multi"
