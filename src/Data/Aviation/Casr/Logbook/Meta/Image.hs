{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.Image(
  Image(Image)
, HasImage(..)
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Meta.ImageType
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data Image =
  Image {
    _imageuri :: String
  , _imagetype :: ImageType
  , _imagesource :: Maybe String
  , _imagename :: Maybe String
  } deriving (Eq, Ord, Show)
  
makeClassy ''Image
