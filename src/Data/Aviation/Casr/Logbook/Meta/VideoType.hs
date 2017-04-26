{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.VideoType(
  VideoType(YouTube, Vimeo, Bambuser)
, AsVideoType(..)
, linkVideoType
, iframeVideoType
) where

import Control.Lens(makeClassyPrisms)
import Data.Eq(Eq)
import Data.List((++))
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data VideoType =
  YouTube
  | Vimeo
  | Bambuser
  deriving (Eq, Ord, Show)
  
makeClassyPrisms ''VideoType

linkVideoType ::
  VideoType
  -> String
  -> String
linkVideoType YouTube u =
  "https://www.youtube.com/watch?v=" ++ u
linkVideoType Vimeo u =
  "https://bambuser.com/v/" ++ u 
linkVideoType Bambuser u =
  "https://vimeo.com/" ++ u

iframeVideoType ::
  VideoType
  -> String
  -> String
iframeVideoType YouTube u =
  "http://www.youtube.com/embed/" ++ u ++ "?autohide=1&amp;cc_load_policy=1&amp;color=white&amp;controls=1&amp;disablekb=0&amp;fs=1&amp;iv_load_policy=0&amp;loop=0&amp;modestbranding=1&amp;rel=0&amp;showinfo=0"
iframeVideoType Vimeo u =
  "https://player.vimeo.com/video/" ++ u
iframeVideoType Bambuser u =
  "https://embed.bambuser.com/broadcast/" ++ u ++ "?chat=1&amp;mute=0"
