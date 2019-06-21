{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Reports.SimulatorTimeReport(
  SimulatorTimeReport(..)
, HasSimulatorTimeReport(..)
, singleSimulatorTimeReport
, getSimulatorTimeReport
) where

import Control.Category((.))
import Control.Lens(makeClassy, (^.))
import Data.Aviation.Casr.Logbook.Types
  (
    TimeAmount
  , Logbook(Logbook)
  , Entry(SimulatorFlightEntry)
  , Entries(Entries)
  , simulatorTime
  , instrumentsimulatorTime
  )
import Data.Eq(Eq)
import Data.Foldable(foldl')
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Prelude(Show)

data SimulatorTimeReport =
  SimulatorTimeReport {
    _hoursTotalSimulator ::
      TimeAmount
  , _hoursInstrumentSimulator ::
      TimeAmount
  }
  deriving (Eq, Ord, Show)

makeClassy ''SimulatorTimeReport

instance Semigroup SimulatorTimeReport where
  SimulatorTimeReport t1 i1 <> SimulatorTimeReport t2 i2 =
    SimulatorTimeReport (t1 <> t2) (i1 <> i2)

instance Monoid SimulatorTimeReport where
  mempty =
    SimulatorTimeReport
      mempty
      mempty
  mappend =
    (<>)

singleSimulatorTimeReport ::
  Entry a b c d
  -> SimulatorTimeReport
singleSimulatorTimeReport (SimulatorFlightEntry fl _) =
  SimulatorTimeReport
    (fl ^. simulatorTime)
    (fl ^. instrumentsimulatorTime)
singleSimulatorTimeReport _ =
  mempty

getSimulatorTimeReport ::
  Logbook a b c d
  -> SimulatorTimeReport
getSimulatorTimeReport (Logbook _ (Entries es)) =
  foldl' (\a -> mappend a . singleSimulatorTimeReport) mempty es
