module Data.Aviation.Casr.Logbook.PoB (
  PoB(..)
, solo
, dual
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown

data PoB =
  PoB Int
  deriving (Eq, Ord, Show)

solo ::
  PoB
solo =
  PoB 1

dual ::
  PoB
dual =
  PoB 2

instance Markdown PoB where  
  markdown (PoB n) =
    concat
      [
        "* PoB: **`"
      , case n of
          0 -> "unmanned"
          1 -> "solo"
          2 -> "dual"
          _ -> show n
      , "`**\n"
      ]
