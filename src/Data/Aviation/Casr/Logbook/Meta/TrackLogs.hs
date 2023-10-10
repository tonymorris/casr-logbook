{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casr.Logbook.Meta.TrackLogs(
  TrackLogs(TrackLogs)
, HasTrackLogs(..)
) where

import Control.Lens(makeClassy, makeWrapped)
import Data.Aviation.Casr.Logbook.Meta.TrackLog ( TrackLog )
import Data.Eq(Eq)
import Data.Monoid ( (<>), Monoid(mempty) )
import Data.Ord(Ord)
import Data.Semigroup ( Semigroup )
import Prelude(Show)

newtype TrackLogs =
  TrackLogs
    [TrackLog]
  deriving (Eq, Ord, Show)

makeClassy ''TrackLogs
makeWrapped ''TrackLogs

instance Semigroup TrackLogs where
  TrackLogs x <> TrackLogs y =
    TrackLogs (x <> y)

instance Monoid TrackLogs where
  mempty =
    TrackLogs mempty
