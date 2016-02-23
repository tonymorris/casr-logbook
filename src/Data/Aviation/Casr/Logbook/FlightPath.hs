module Data.Aviation.Casr.Logbook.FlightPath (
  FlightPath(..)
, directPath
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown

data FlightPath =
  FlightPath
    String -- start
    [String] -- touch-downs
    String -- end
  deriving (Eq, Ord, Show)

directPath ::
  String
  -> String
  -> FlightPath
directPath fr to =
  FlightPath
    fr
    []
    to

instance Markdown FlightPath where
  markdown (FlightPath s x e) =
    concat
      [
        "* Path: **"
      , s
      , x >>= (" - " ++)
      , " - "
      , e
      , "**\n"
      ]
