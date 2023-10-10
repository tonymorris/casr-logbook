{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.Visualisation(
  Visualisation(Doarama)
, HasVisualisation(..)
, doaramaname'
) where

import Control.Category ( Category((.)) )
import Control.Lens(makeClassy, Traversal', _Just)
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data Visualisation =
  Doarama {
    _doaramaid :: String
  , _oembedid :: String
  , _doaramaname :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Visualisation

doaramaname' ::
  HasVisualisation c =>
  Traversal' c String
doaramaname' =
  doaramaname . _Just
