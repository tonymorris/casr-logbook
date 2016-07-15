{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Exam(
  Exam(..)
, dayonlyexam
) where

import Control.Lens(makeClassy)
import Data.Aviation.Casr.Logbook.Aviator(Aviator)
import Data.Aviation.Casr.Logbook.Time(Time, dayonly)
import Data.Aviation.Casr.Logbook.Location(Location)
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
