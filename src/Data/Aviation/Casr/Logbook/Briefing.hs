{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Briefing(
  Briefing(..)
) where
  
import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.Location(Location)
import Data.Aviation.Casr.Logbook.Time(Time)
import Data.Aviation.Casr.Logbook.TimeAmount(TimeAmount)
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
