module Data.Aviation.Casr.Logbook.Visualisations (
  Visualisations(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Visualisation

newtype Visualisations =
  Visualisations
    [Visualisation]
  deriving (Eq, Ord, Show)
    
instance Monoid Visualisations where
  mempty =
    Visualisations []
  Visualisations v1 `mappend` Visualisations v2 =
    Visualisations (v1 `mappend` v2)
       
instance Markdown Visualisations where
  markdown (Visualisations v) =
    case v of
      [] ->
        ""
      _ ->
        "* **Visualisations**\n" ++ (v >>= \w -> "  * " ++ markdown w ++ "\n")
