{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Casr.Logbook.Entries(
  Entries(..)
, emptyentries
, singleentry
) where

import Control.Lens(makeWrapped)
import Data.Aviation.Casr.Logbook.Entry(Entry)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)
  
newtype Entries ae se ee be =
  Entries
    [Entry ae se ee be]
  deriving (Eq, Ord, Show)

makeWrapped ''Entries

emptyentries ::
  Entries ae se ee be
emptyentries =
  Entries []

singleentry ::
  Entry ae se ee be
  -> Entries ae se ee be
singleentry e =
  Entries [e]
