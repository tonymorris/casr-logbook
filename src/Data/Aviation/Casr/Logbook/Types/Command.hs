{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Command(
  Command(..)
, AsCommand(..)
, getUnderInstructionPic
, isAeronauticalHours
, _InCommandIncludingInstructing
) where

import Control.Applicative
    ( Applicative((*>)), Alternative((<|>)) )
import Control.Lens
    ( Prism', preview, makeClassyPrisms, isn't, prism', (#) )
import Data.Aviation.Casr.Logbook.Types.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.Types.Instruction ( Instruction )
import Data.Bool ( Bool(..), not )
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
  AsCommand c =>
  c
  -> Bool
isAeronauticalHours c =
  not (isn't _ApprovedSolo c)

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
        preview _InCommand c *> Nothing <|>
        preview _InCommandIncludingInstructing c
    )
