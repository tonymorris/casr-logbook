{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.Image(
  Image(Image)
, HasImage(..)
, imageUriType
, imageurismall'
, imagesource'
, imagename'
) where

import Control.Category ( Category((.)) )
import Control.Lens(makeClassy, Traversal', _Just)
import Data.Aviation.Casr.Logbook.Meta.ImageType ( ImageType )
import Data.Eq(Eq)
import Data.Maybe(Maybe(..))
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data Image =
  Image {
    _imageuri :: String
  , _imageurismall :: Maybe String
  , _imagetype :: ImageType
  , _imagesource :: Maybe String
  , _imagename :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Image

imageUriType ::
  String
  -> ImageType
  -> Image
imageUriType s t =
  Image s Nothing t Nothing Nothing

imageurismall' ::
  HasImage c => Traversal' c String
imageurismall' =
  imageurismall . _Just

imagesource' ::
  HasImage c => Traversal' c String
imagesource' =
  imagesource . _Just

imagename' ::
  HasImage c => Traversal' c String
imagename' =
  imagename . _Just
