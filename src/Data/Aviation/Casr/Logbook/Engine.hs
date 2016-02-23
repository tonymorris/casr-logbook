module Data.Aviation.Casr.Logbook.Engine (
  Engine(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown

data Engine =
  Single
  | Multi
  deriving (Eq, Ord, Show)
    
instance Markdown Engine where
  markdown Single =
    "single"
  markdown Multi =
    "multi"
