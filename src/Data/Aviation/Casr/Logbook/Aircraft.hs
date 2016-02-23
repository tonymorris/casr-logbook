module Data.Aviation.Casr.Logbook.Aircraft (
  Aircraft(..)
) where

import Data.Aviation.Casr.Logbook.Engine
import Data.Aviation.Casr.Logbook.Printer.Markdown

data Aircraft =
  Aircraft
    String -- type
    String -- registration
    Engine
  deriving (Eq, Ord, Show)

instance Markdown Aircraft where
  markdown (Aircraft t r e) =
    concat
      [
        "* Aircraft"
      , "\n  * Type: **"
      , t
      , "**\n  * Registration: **`"
      , r
      , "`**\n  * Engine: **`"
      , markdown e
      , "`**\n"
      ]
