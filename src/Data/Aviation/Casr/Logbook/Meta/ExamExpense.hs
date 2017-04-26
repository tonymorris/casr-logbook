{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.ExamExpense(
  ExamExpense(ExamExpense)
, HasExamExpense(..)
) where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data ExamExpense =
  ExamExpense {
    _examexpenseamount :: Int
  , _examexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''ExamExpense
