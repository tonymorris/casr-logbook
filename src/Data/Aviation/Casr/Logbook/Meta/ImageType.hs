{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.ImageType(
  ImageType(Jpg, Png, Gif)
, AsImageType(..)
, jpg
, png
, gif
) where

import Control.Lens(makeClassyPrisms, ( # ))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data ImageType =
  Jpg
  | Png
  | Gif
  deriving (Eq, Ord, Show)

makeClassyPrisms ''ImageType

jpg ::
  AsImageType t =>
  t
jpg =
  _Jpg # ()

png ::
  AsImageType t =>
  t
png =
  _Png # ()

gif ::
  AsImageType t =>
  t
gif =
  _Gif # ()
