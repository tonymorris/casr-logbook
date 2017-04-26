{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.Video(
  Video(Video)
, HasVideo(..)
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Meta.VideoType
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data Video =
  Video {
    _videouri :: String
  , _videotype :: VideoType
  , _videosource :: Maybe String
  , _videoname :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Video
