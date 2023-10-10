{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Reports.Meta.ExpenseReport(
  ExpenseReport(ExpenseReport)
, HasExpenseReport(..)
, entryExpenseReport
, logbookExpenseReport
) where

import Control.Category((.))
import Control.Lens(makeClassy, (^.), _Wrapped)
import Control.Monad((>>=), return)
import Data.Aviation.Casr.Logbook.Types.Entry
    ( Entry(AircraftFlightEntry, SimulatorFlightEntry, ExamEntry,
            BriefingEntry) )
import Data.Aviation.Casr.Logbook.Types.Logbook
    ( Logbook, logbookentries )
import Data.Aviation.Casr.Logbook.Meta.AircraftFlightExpense
    ( AircraftFlightExpense(ExpenseAircraftUsage,
                            ExpenseAircraftLanding) )
import Data.Aviation.Casr.Logbook.Meta.AircraftFlightMeta
    ( AircraftFlightMeta, expenses )
import Data.Aviation.Casr.Logbook.Meta.AircraftLandingExpense
    ( aircraftlandingexpenseamount )
import Data.Aviation.Casr.Logbook.Meta.AircraftUsageExpense
    ( aircraftUsageCost )
import Data.Aviation.Casr.Logbook.Meta.BriefingExpense
    ( briefingCost )
import Data.Aviation.Casr.Logbook.Meta.BriefingMeta
    ( BriefingMeta )
import Data.Aviation.Casr.Logbook.Meta.ExamExpense
    ( examexpenseamount )
import Data.Aviation.Casr.Logbook.Meta.ExamMeta ( ExamMeta )
import Data.Aviation.Casr.Logbook.Meta.SimulatorFlightExpense
    ( simulatorFlightCost )
import Data.Aviation.Casr.Logbook.Meta.SimulatorFlightMeta
    ( SimulatorFlightMeta )
import Data.Eq(Eq)
import Data.Foldable(sum, foldMap)
import Data.Function(($))
import Data.Int(Int)
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Prelude(Show, (+))

data ExpenseReport =
  ExpenseReport {
    _aircraftUsageExpenseTotal :: Int
  , _aircraftLandingExpenseTotal :: Int
  , _briefingExpenseTotal :: Int
  , _examExpenseTotal :: Int
  , _simulatorFlightTotal :: Int
  } deriving (Eq, Ord, Show)

makeClassy ''ExpenseReport

instance Semigroup ExpenseReport where
  ExpenseReport ag1 al1 b1 e1 s1 <> ExpenseReport ag2 al2 b2 e2 s2 =
    ExpenseReport (ag1 + ag2) (al1 + al2) (b1 + b2) (e1 + e2) (s1 + s2)

instance Monoid ExpenseReport where
  mempty =
    ExpenseReport 0 0 0 0 0
  mappend =
    (<>)

entryExpenseReport ::
  Entry AircraftFlightMeta SimulatorFlightMeta ExamMeta BriefingMeta
  -> ExpenseReport
entryExpenseReport (AircraftFlightEntry fl e) =
  let usage =
        sum $ e ^. expenses >>= \p ->
          case p of
            ExpenseAircraftUsage e' -> [aircraftUsageCost fl e']
            ExpenseAircraftLanding _ -> []
      landing =
        sum $ e ^. expenses >>= \p ->
          case p of
            ExpenseAircraftUsage _ -> []
            ExpenseAircraftLanding l -> [l ^. aircraftlandingexpenseamount]
  in  ExpenseReport
        usage
        landing
        0
        0
        0
entryExpenseReport (SimulatorFlightEntry fl e) =
  ExpenseReport
    0
    0
    0
    0
    (sum $ e ^. _Wrapped >>= return . simulatorFlightCost fl)
entryExpenseReport (ExamEntry _ e) =
  ExpenseReport
    0
    0
    0
    (sum $ e ^. _Wrapped >>= return . (^. examexpenseamount))
    0
entryExpenseReport (BriefingEntry br e) =
  ExpenseReport
    0
    0
    (sum $ e ^. _Wrapped >>= return . briefingCost br)
    0
    0

logbookExpenseReport ::
  Logbook AircraftFlightMeta SimulatorFlightMeta ExamMeta BriefingMeta
  -> ExpenseReport
logbookExpenseReport b =
  foldMap entryExpenseReport (b ^. logbookentries . _Wrapped)

