{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.Video(
  Video(Video)
, HasVideo(..)
, videoUriType
, videosource'
, videoname'
) where

import Control.Category
import Control.Lens(makeClassy, Traversal', _Just)
import Data.Aviation.Casr.Logbook.Meta.VideoType ( VideoType )
import Data.Eq(Eq)
import Data.Maybe(Maybe(..))
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

videoUriType ::
  String
  -> VideoType
  -> Video
videoUriType s t =
  Video s t Nothing Nothing

videosource' ::
  HasVideo c =>
  Traversal' c String
videosource' =
  videosource . _Just

videoname' ::
  HasVideo c =>
  Traversal' c String
videoname' =
  videoname . _Just
