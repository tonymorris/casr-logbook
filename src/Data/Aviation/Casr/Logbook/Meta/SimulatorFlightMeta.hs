{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casr.Logbook.Meta.SimulatorFlightMeta(
  SimulatorFlightMeta(SimulatorFlightMeta)
, HasSimulatorFlightMeta(..)
) where

import Control.Lens(makeClassy, makeWrapped)
import Data.Aviation.Casr.Logbook.Meta.SimulatorFlightExpense
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

newtype SimulatorFlightMeta =
  SimulatorFlightMeta
    [SimulatorFlightExpense]
  deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightMeta
makeWrapped ''SimulatorFlightMeta
