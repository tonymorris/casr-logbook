module Data.Aviation.Casr.Logbook.ARN (
  ARN(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
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
    "* Aviation Reference Number: **`" ++ markdown s ++ "`**\n"

instance Html ARN where
  html (ARN s) =
    "<span class=\"heading arnheading\">ARN: </span><span class=\"arninfo\">" ++ html s ++ "</span>"
