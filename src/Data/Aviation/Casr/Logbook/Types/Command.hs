{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Command(
  Command(..)
, AsCommand(..)
, getUnderInstructionPic
, isAeronauticalHours
) where

import Control.Lens(makeClassyPrisms)
import Data.Aviation.Casr.Logbook.Types.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.Types.Instruction ( Instruction )
import Data.Bool ( Bool(..) )
import Data.Eq(Eq)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Prelude(Show)

data Command =
  ICUS Aviator
  | Dual Aviator
  | InCommand
  | InCommandInstructing Instruction
  | ApprovedSolo Aviator
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Command

getUnderInstructionPic ::
  Command
  -> Maybe Aviator
getUnderInstructionPic (ICUS a) =
  Just a
getUnderInstructionPic (Dual a) =
  Just a
getUnderInstructionPic InCommand =
  Nothing
getUnderInstructionPic (InCommandInstructing _) =
  Nothing
getUnderInstructionPic (ApprovedSolo _) =
  Nothing

isAeronauticalHours ::
  Command
  -> Bool
isAeronauticalHours (ICUS _) =
  True
isAeronauticalHours (Dual _) =
  True
isAeronauticalHours InCommand =
  True
isAeronauticalHours (InCommandInstructing _) =
  True
isAeronauticalHours (ApprovedSolo _) =
  False
