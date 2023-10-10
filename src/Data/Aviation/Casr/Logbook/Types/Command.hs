{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Command(
  Command(..)
, AsCommand(..)
, getUnderInstructionPic
, _InCommandIncludingInstructing
) where

import Control.Applicative
    ( Alternative((<|>)) )
import Control.Lens
    ( Prism', preview, makeClassyPrisms, prism', (#) )
import Data.Aviation.Casr.Logbook.Types.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.Types.Instruction ( Instruction, InstructionRating )
import Data.Eq(Eq)
import Data.Functor ( Functor((<$)), (<$>) )
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Prelude(Show)

data Command =
  ICUS Aviator
  | Dual Aviator
  | InCommand
  | InCommandInstructing Instruction
  | ApprovedSolo Aviator InstructionRating
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
getUnderInstructionPic (ApprovedSolo a _)=
  Just a

_InCommandIncludingInstructing ::
  AsCommand c =>
  Prism' c (Maybe Instruction)
_InCommandIncludingInstructing =
  prism'
    (\case
      Nothing ->
        _InCommand # ()
      Just i ->
        _InCommandInstructing # i
    )
    (
      \c ->
        (Nothing <$ preview _InCommand c) <|>
        (Just <$> preview _InCommandInstructing c)
    )
