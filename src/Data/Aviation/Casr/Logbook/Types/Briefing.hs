{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Briefing(
  Briefing(..)
, HasBriefing(..)
) where

import Control.Category((.))
import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Types.Aviator(Aviator, HasAviator(aviator))
import Data.Aviation.Casr.Logbook.Types.Location(Location, HasLocation(location))
import Data.Aviation.Casr.Logbook.Types.Time(Time, HasTime(time))
import Data.Aviation.Casr.Logbook.Types.TimeAmount(TimeAmount, HasTimeAmount(timeAmount))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data Briefing =
  Briefing {
    _briefingName :: String
  , _briefingLocation :: Location
  , _briefingTime :: Time
  , _briefer :: Aviator
  , _briefingTimeAmount :: TimeAmount
  } deriving (Eq, Ord, Show)

makeClassy ''Briefing

instance HasLocation Briefing where
  location =
    briefingLocation . location

instance HasTime Briefing where
  time =
    briefingTime .  time

instance HasAviator Briefing where
  aviator =
    briefer . aviator

instance HasTimeAmount Briefing where
  timeAmount =
    briefingTimeAmount . timeAmount

