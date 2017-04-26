{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Location(
  Location(..)
, HasLocation(..)
) where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show, Double)

data Location =
  Location {
    _locationname :: String
  , _locationlatitude :: Double
  , _locationlongitude :: Double
  } deriving (Eq, Ord, Show)

makeClassy ''Location
