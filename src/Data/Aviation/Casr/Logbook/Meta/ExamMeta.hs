{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aviation.Casr.Logbook.Meta.ExamMeta(
  ExamMeta(ExamMeta)
, HasExamMeta(..)
) where

import Control.Lens(makeClassy, makeWrapped)
import Data.Aviation.Casr.Logbook.Meta.ExamExpense ( ExamExpense )
import Data.Eq(Eq)
import Data.Monoid ( (<>), Monoid(mempty) )
import Data.Ord(Ord)
import Data.Semigroup ( Semigroup )
import Prelude(Show)

newtype ExamMeta =
  ExamMeta
    [ExamExpense]
  deriving (Eq, Ord, Show)

makeClassy ''ExamMeta
makeWrapped ''ExamMeta

instance Semigroup ExamMeta where
  ExamMeta x <> ExamMeta y =
    ExamMeta (x <> y)

instance Monoid ExamMeta where
  mempty =
    ExamMeta mempty
