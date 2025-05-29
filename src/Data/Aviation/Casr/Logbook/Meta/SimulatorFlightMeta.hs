{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aviation.Casr.Logbook.Meta.SimulatorFlightMeta(
  SimulatorFlightMeta(SimulatorFlightMeta)
, HasSimulatorFlightMeta(..)
) where

import Control.Lens(makeClassy, makeWrapped)
import Data.Aviation.Casr.Logbook.Meta.SimulatorFlightExpense
    ( SimulatorFlightExpense )
import Data.Eq(Eq)
import Data.Monoid ( (<>), Monoid(mempty) )
import Data.Ord(Ord)
import Data.Semigroup ( Semigroup )
import Prelude(Show)

newtype SimulatorFlightMeta =
  SimulatorFlightMeta
    [SimulatorFlightExpense]
  deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightMeta
makeWrapped ''SimulatorFlightMeta

instance Semigroup SimulatorFlightMeta where
  SimulatorFlightMeta x <> SimulatorFlightMeta y =
    SimulatorFlightMeta (x <> y)

instance Monoid SimulatorFlightMeta where
  mempty :: SimulatorFlightMeta
  mempty =
    SimulatorFlightMeta mempty
