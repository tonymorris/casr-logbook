{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.BriefingExpense(
  BriefingExpense(BriefingExpense)
, HasBriefingExpense(..)
, briefingCost
) where

import Control.Lens(makeClassy, (^.))
import Data.Aviation.Casr.Logbook.Types.Briefing(HasBriefing, briefingTimeAmount)
import Data.Aviation.Casr.Logbook.Types.TimeAmount(timeAmountBy10)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show, (*))

data BriefingExpense =
  BriefingExpense {
    _briefingexpenseperhour :: Int
  , _briefingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''BriefingExpense

briefingCost ::
  HasBriefing s =>
  s
  -> BriefingExpense
  -> Int
briefingCost br (BriefingExpense perhour _) =
  let z = br ^. briefingTimeAmount
  in  timeAmountBy10 z * perhour
