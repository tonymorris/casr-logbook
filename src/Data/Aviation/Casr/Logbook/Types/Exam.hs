{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Exam(
  Exam(..)
, HasExam(..)
, dayonlyexam
) where

import Control.Category((.))
import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Types.Aviator(Aviator, HasAviator(aviator))
import Data.Aviation.Casr.Logbook.Types.Time(Time, HasTime(time), dayonly)
import Data.Aviation.Casr.Logbook.Types.Location(Location, HasLocation(location))
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Data.Time(Day)
import Prelude(Show)

data Exam =
  Exam {
    _examName :: String
  , _examLocation :: Location
  , _examTime :: Time
  , _examDelegate :: Aviator
  , _examResult :: Int
  , _examResultMaximum :: Int
  } deriving (Eq, Ord, Show)

makeClassy ''Exam

instance HasLocation Exam where
  location =
    examLocation . location

instance HasTime Exam where
  time =
    examTime . time

instance HasAviator Exam where
  aviator =
    examDelegate . aviator

dayonlyexam ::
  String
  -> Location
  -> Day
  -> Aviator
  -> Int
  -> Int
  -> Exam
dayonlyexam n l d =
  Exam
    n
    l
    (dayonly d)
