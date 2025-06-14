{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft(
  Aircraft(..)
, HasAircraft(..)
, singleaircraft
, multiaircraft
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Types.Engine(Engine(Single, Multi))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data Aircraft =
  Aircraft {
    _aircraftYearModel :: String
  , _aircraftType :: String
  , _aircraftTypeVariant :: String
  , _aircraftRegistration :: String
  , _aircraftEngine :: Engine
  } deriving (Eq, Ord, Show)

makeClassy ''Aircraft

singleaircraft ::
  String
  -> String
  -> String
  -> String
  -> Aircraft
singleaircraft y t v r =
  Aircraft
    y
    t
    v
    r
    Single

multiaircraft ::
  String
  -> String
  -> String
  -> String
  -> Aircraft
multiaircraft y t v r =
  Aircraft
    y
    t
    v
    r
    Multi
