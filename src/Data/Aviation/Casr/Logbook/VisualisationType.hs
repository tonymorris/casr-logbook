module Data.Aviation.Casr.Logbook.VisualisationType (
  VisualisationType(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Html
import Data.Aviation.Casr.Logbook.Printer.Markdown

data VisualisationType =
  Doarama
    String -- http://oembed.frdnspnzr.de/
  deriving (Eq, Ord, Show)

instance Markdown VisualisationType where
  markdown (Doarama _) =
    "doarama"

instance Html VisualisationType where
  html (Doarama _) =
    "doarama"
