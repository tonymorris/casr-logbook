{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Instruction where

import Control.Lens ( makeClassyPrisms, isn't, makeClassy )
import Data.Aviation.Casr.Logbook.Types.Aviator ( Aviator )
import Data.Bool ( Bool, (&&) )
import Data.Eq(Eq)
import Data.Functor ( Functor(fmap) )
import Data.Maybe ( Maybe )
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data InstructionLesson =
  InstructionLesson {
    _student :: Aviator
  , _lesson :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''InstructionLesson

data InstructionRating =
  GA3InstructionRating
  | GA2InstructionRating
  | GA1InstructionRating
  | RAInstructionRating
  | RASIInstructionRating
  deriving (Eq, Ord, Show)

makeClassy ''InstructionRating
makeClassyPrisms ''InstructionRating

isRAInstruction ::
  AsInstructionRating a =>
  a
  -> Bool
isRAInstruction x =
  isn't _GA1InstructionRating x &&
  isn't _GA2InstructionRating x &&
  isn't _GA3InstructionRating x

isGAInstruction ::
  AsInstructionRating a =>
  a
  -> Bool
isGAInstruction x =
  isn't _RAInstructionRating x &&
  isn't _RASIInstructionRating x

data Instruction =
  Instruction {
    __instructionLesson :: InstructionLesson
  , __instructionWithRating :: InstructionRating
  } deriving (Eq, Ord, Show)

makeClassy ''Instruction

instance HasInstructionLesson Instruction where
  instructionLesson f (Instruction l r) =
    fmap (`Instruction` r) (f l)

instance HasInstructionRating Instruction where
  instructionRating f (Instruction l r) =
    fmap (Instruction l) (f r)
