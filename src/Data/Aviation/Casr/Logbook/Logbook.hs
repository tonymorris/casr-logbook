{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casr.Logbook.Logbook(
  Logbook(..)
, HasLogbook(..)
, aviatorlogbook
, emptylogbook
, singleentrylogbook
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.Entries(Entries(Entries), emptyentries, singleentry)
import Data.Aviation.Casr.Logbook.Entry(Entry)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data Logbook ae se ee be =
  Logbook {
    _logbookaviator :: Aviator
  , _logbookentries :: (Entries ae se ee be)
  } deriving (Eq, Ord, Show)

makeClassy ''Logbook

aviatorlogbook ::
  Aviator
  -> [Entry ae se ee be]
  -> Logbook ae se ee be
aviatorlogbook a e =
  Logbook
    a
    (Entries e)

emptylogbook ::
  Aviator
  -> Logbook ae se ee be
emptylogbook a =
  Logbook
    a
    emptyentries

singleentrylogbook ::
  Aviator
  -> Entry ae se ee be
  -> Logbook ae se ee be
singleentrylogbook a e =
  Logbook
    a
    (singleentry e)