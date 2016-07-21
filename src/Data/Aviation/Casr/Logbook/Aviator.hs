{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Aviator(
  Aviator(..)
, HasAviator(..)
, aviatorwithname
, nodobaviator
, dobaviator
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Rating(Rating)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Digit(Digit)
import Data.Maybe(Maybe(Nothing, Just))
import Data.String(String)
import Data.Time(Day)
import Prelude(Show)

data Aviator =
  Aviator {
    _surname :: String
  , _firstname :: String
  , _arn :: [Digit]
  , _dob :: Maybe Day
  , _ratings :: [Rating]
  } deriving (Eq, Ord, Show)

makeClassy ''Aviator

aviatorwithname ::
  String
  -> String
  -> Aviator
aviatorwithname s f =
  Aviator
    s
    f
    []
    Nothing
    []

nodobaviator ::
  String
  -> String
  -> [Digit]
  -> [Rating]
  -> Aviator
nodobaviator s f r =
  Aviator
    s 
    f
    r
    Nothing

dobaviator ::
  String
  -> String
  -> [Digit]
  -> Day
  -> [Rating]
  -> Aviator
dobaviator s f r b =
  Aviator
    s 
    f
    r
    (Just b)
