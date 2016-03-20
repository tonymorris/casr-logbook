module Data.Aviation.Casr.Logbook.Hours (
  Hours(..)
, zeroHours
, addHours
, fractionalHours
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown

-- abstract
data Hours =
  Hours
    Int -- full
    Int  -- partial
  deriving (Eq, Ord, Show)

zeroHours ::
  Hours
zeroHours =
  Hours 0 0

addHours ::
  Hours 
  -> Hours
  -> Hours
Hours f1 p1 `addHours` Hours f2 p2 =
  let (h, q) = divMod (p1 + p2) 10
  in  Hours (f1 + f2 + h) q 

instance Monoid Hours where
  mempty =
    zeroHours
  mappend =
    addHours

instance Markdown Hours where
  markdown (Hours t p) =
    "* Hours: **`" ++ show t ++ "." ++ show p ++ "`**\n"

fractionalHours ::
  Fractional a =>
  Hours
  -> a
fractionalHours (Hours f p) =
  fromIntegral f + (fromIntegral p / 10)
