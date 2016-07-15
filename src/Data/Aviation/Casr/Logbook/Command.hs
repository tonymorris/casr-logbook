{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Command(
  Command(..)
) where

import Control.Lens(makeClassyPrisms)
import Data.Aviation.Casr.Logbook.Aviator(Aviator)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data Command =
  ICUS Aviator
  | Dual Aviator
  | InCommand
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Command
