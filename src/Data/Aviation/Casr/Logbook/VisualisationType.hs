module Data.Aviation.Casr.Logbook.VisualisationType (
  VisualisationType(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown

data VisualisationType =
  Doarama
  deriving (Eq, Ord, Show)

instance Markdown VisualisationType where
  markdown Doarama =
    "doarama"
