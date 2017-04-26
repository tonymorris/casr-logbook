{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Casr.Logbook.Meta.Passenger(
  Passenger(Passenger)
, HasPassenger(..)
) where

import Control.Lens(makeWrapped, makeClassy)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

newtype Passenger =
  Passenger
    String
  deriving (Eq, Ord, Show)

makeWrapped '' Passenger
makeClassy '' Passenger
