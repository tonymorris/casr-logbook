{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casr.Logbook.Meta.TrackLogs(
  TrackLogs(TrackLogs)
, HasTrackLogs(..)
) where

import Control.Lens(makeClassy, makeWrapped)
import Data.Aviation.Casr.Logbook.Meta.TrackLog
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

newtype TrackLogs =
  TrackLogs
    [TrackLog]
  deriving (Eq, Ord, Show)

makeClassy ''TrackLogs
makeWrapped ''TrackLogs
