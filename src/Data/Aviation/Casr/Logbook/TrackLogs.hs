module Data.Aviation.Casr.Logbook.TrackLogs (
  TrackLogs(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.Aviation.Casr.Logbook.TrackLog

newtype TrackLogs =
  TrackLogs
    [TrackLog]
  deriving (Eq, Ord, Show)

instance Monoid TrackLogs where
  mempty =
    TrackLogs []
  TrackLogs t1 `mappend` TrackLogs t2 =
    TrackLogs (t1 `mappend` t2)

instance Markdown TrackLogs where
  markdown (TrackLogs t) =
    case t of
      [] ->
        ""
      _ ->
        "* **Track**\n" ++ (t >>= \u -> markdown u ++ "\n")

instance Html TrackLogs where
  html (TrackLogs t) =
    case t of
      [] ->
        ""
      _ ->
        concat
          [
            "<span class=\"heading tracklogheading\">"
          , "Track"
          , "</span>"
          , "<ul>"
          , t >>= \u -> concat
                          [
                            "<li>"
                          , html u
                          , "</li>"
                          ]
          , "</ul>"
          ]
        