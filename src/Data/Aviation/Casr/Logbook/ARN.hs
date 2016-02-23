module Data.Aviation.Casr.Logbook.ARN (
  ARN(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.String

newtype ARN =
  ARN
    String
  deriving (Eq, Ord, Show)

instance IsString ARN where
  fromString =
    ARN
    
instance Markdown ARN where
  markdown (ARN s) =
    "* Aviation Reference Number: **`" ++ s ++ "`**\n"
