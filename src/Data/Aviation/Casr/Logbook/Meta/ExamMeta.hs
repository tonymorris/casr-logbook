{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casr.Logbook.Meta.ExamMeta(
  ExamMeta(ExamMeta)
, HasExamMeta(..)
) where

import Control.Lens(makeClassy, makeWrapped)
import Data.Aviation.Casr.Logbook.Meta.ExamExpense
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

newtype ExamMeta =
  ExamMeta
    [ExamExpense]
  deriving (Eq, Ord, Show)

makeClassy ''ExamMeta
makeWrapped ''ExamMeta
