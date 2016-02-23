module Data.Aviation.Casr.Logbook.Name (
  Name(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.String

newtype Name =
  Name
    String
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString =
    Name

instance Markdown Name where
  markdown (Name s) =
    "### " ++ s ++ "\n"
