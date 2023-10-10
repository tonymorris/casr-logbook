{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.TrackLog(
  TrackLog(TrackLog)
, HasTrackLog(..)
, tracklogUriType
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Meta.TrackLogType
    ( TrackLogType )
import Data.Eq(Eq)
import Data.Maybe(Maybe(..))
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data TrackLog =
  TrackLog {
    _trackloguri :: String
  , _tracklogtype :: TrackLogType
  , _tracklogsource :: Maybe String
  , _tracklogname :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''TrackLog

tracklogUriType ::
  String
  -> TrackLogType
  -> TrackLog
tracklogUriType s t =
  TrackLog s t Nothing Nothing
