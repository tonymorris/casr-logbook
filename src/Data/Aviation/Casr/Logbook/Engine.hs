{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Engine(
  Engine(..)
) where

import Control.Lens(makeClassyPrisms)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data Engine =
  Single
  | Multi
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Engine
