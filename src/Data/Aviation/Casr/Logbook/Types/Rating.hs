{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Rating(
  Rating(..)
, HasRating(..)
, nodayrating
, dayrating
) where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.String(String)
import Data.Time(Day)
import Prelude(Show)

data Rating =
  Rating {
    _ratingName :: String
  , _ratingAchieved :: Maybe Day
  } deriving (Eq, Ord, Show)

makeClassy ''Rating

nodayrating ::
  String
  -> Rating
nodayrating r =
  Rating
    r
    Nothing

dayrating ::
  String
  -> Day
  -> Rating
dayrating r d =
  Rating
    r
    (Just d)
