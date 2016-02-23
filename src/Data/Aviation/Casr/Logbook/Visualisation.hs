module Data.Aviation.Casr.Logbook.Visualisation (
  Visualisation(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.VisualisationType

data Visualisation =
  Visualisation
    String -- uri
    (Maybe String) -- name
    VisualisationType
  deriving (Eq, Ord, Show)

instance Markdown Visualisation where
  markdown (Visualisation uri name vtype) =
    let t = markdown vtype
        n = case name of
              Nothing -> ""
              Just n' -> "**" ++ n' ++ "**: "
    in  concat
          [
            n
          , "["
          , t
          , "]("
          , uri
          , ")"
          ]
