module Data.Aviation.Casr.Logbook.Videos (
  Videos(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Video

newtype Videos =
  Videos
    [Video]
  deriving (Eq, Ord, Show)
    
instance Monoid Videos where
  mempty =
    Videos []
  Videos v1 `mappend` Videos v2 =
    Videos (v1 `mappend` v2)

instance Markdown Videos where
  markdown (Videos v) =
    case v of
      [] ->
        ""
      _ ->
        "* **Videos**\n" ++ (v >>= \w -> "  * " ++ markdown w ++ "\n")
