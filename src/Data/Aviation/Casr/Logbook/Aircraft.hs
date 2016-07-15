{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Aircraft(
  Aircraft(..)
, singleaircraft
, multiaircraft
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Engine(Engine(Single, Multi))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data Aircraft =
  Aircraft {
    _aircraftType :: String
  , _aircraftRegistration :: String
  , _aircraftEngine :: Engine
  } deriving (Eq, Ord, Show)

makeClassy ''Aircraft

singleaircraft ::
  String
  -> String
  -> Aircraft
singleaircraft t r =
  Aircraft
    t
    r
    Single

multiaircraft ::
  String
  -> String
  -> Aircraft
multiaircraft t r =
  Aircraft
    t
    r
    Multi
