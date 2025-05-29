{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aviation.Casr.Logbook.Meta.BriefingMeta(
  BriefingMeta(BriefingMeta)
, HasBriefingMeta(..)
) where

import Control.Lens(makeClassy, makeWrapped)
import Data.Aviation.Casr.Logbook.Meta.BriefingExpense
    ( BriefingExpense )
import Data.Eq(Eq)
import Data.Monoid ( (<>), Monoid(mempty) )
import Data.Ord(Ord)
import Data.Semigroup ( Semigroup )
import Prelude(Show)

newtype BriefingMeta =
  BriefingMeta
    [BriefingExpense]
  deriving (Eq, Ord, Show)

makeClassy ''BriefingMeta
makeWrapped ''BriefingMeta

instance Semigroup BriefingMeta where
  BriefingMeta x <> BriefingMeta y =
    BriefingMeta (x <> y)

instance Monoid BriefingMeta where
  mempty =
    BriefingMeta mempty
