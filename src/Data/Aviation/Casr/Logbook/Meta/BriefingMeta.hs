{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casr.Logbook.Meta.BriefingMeta(
  BriefingMeta(BriefingMeta)
, HasBriefingMeta(..)
) where

import Control.Lens(makeClassy, makeWrapped)
import Data.Aviation.Casr.Logbook.Meta.BriefingExpense
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

newtype BriefingMeta =
  BriefingMeta
    [BriefingExpense]
  deriving (Eq, Ord, Show)

makeClassy ''BriefingMeta
makeWrapped ''BriefingMeta
