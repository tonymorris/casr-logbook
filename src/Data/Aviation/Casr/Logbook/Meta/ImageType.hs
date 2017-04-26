{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.ImageType(
  ImageType(Jpg, Png, Gif)
, AsImageType(..)
) where

import Control.Lens(makeClassyPrisms)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data ImageType =
  Jpg
  | Png
  | Gif
  deriving (Eq, Ord, Show)

makeClassyPrisms ''ImageType
