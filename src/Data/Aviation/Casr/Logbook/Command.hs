{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Command(
  Command(..)
, AsCommand(..)
, getInstructingPic
) where

import Control.Lens(makeClassyPrisms)
import Data.Aviation.Casr.Logbook.Aviator(Aviator)
import Data.Eq(Eq)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Prelude(Show)

data Command =
  ICUS Aviator
  | Dual Aviator
  | InCommand
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Command

getInstructingPic ::
  Command
  -> Maybe Aviator
getInstructingPic (ICUS a) =
  Just a
getInstructingPic (Dual a) =
  Just a
getInstructingPic InCommand =
  Nothing
