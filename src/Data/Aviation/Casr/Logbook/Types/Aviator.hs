{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aviator(
  Aviator(..)
, HasAviator(..)
, aviatorwithname
, nodobaviator
, dobaviator
, dob'
) where

import Control.Category ( Category((.)) )
import Control.Lens(makeClassy, Traversal', _Just)
import Data.Aviation.Casr.Logbook.Types.Rating(Rating)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Digit(DecDigit)
import Data.Maybe(Maybe(Nothing, Just))
import Data.String(String)
import Data.Time(Day)
import Prelude(Show)

data Aviator =
  Aviator {
    _surname :: String
  , _firstname :: String
  , _arn :: [DecDigit]
  , _dob :: Maybe Day
  , _ratings :: [Rating]
  } deriving (Eq, Ord, Show)

makeClassy ''Aviator

dob' ::
  HasAviator c =>
  Traversal' c Day
dob' =
  dob . _Just

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
  -> [DecDigit]
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
  -> [DecDigit]
  -> Day
  -> [Rating]
  -> Aviator
dobaviator s f r b =
  Aviator
    s
    f
    r
    (Just b)
