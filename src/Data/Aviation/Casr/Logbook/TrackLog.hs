module Data.Aviation.Casr.Logbook.TrackLog (
  TrackLog(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.TrackLogType
import Data.Maybe

data TrackLog =
  TrackLog
    String -- uri
    (Maybe String) -- name
    TrackLogType
  deriving (Eq, Ord, Show)

instance Markdown TrackLog where
  markdown (TrackLog uri name ttype) =
    let t = markdown ttype        
        n = fmap (\z -> "**" ++ z ++ "**") name
    in  case ttype of 
          ImageLog _ ->
            concat
              [
                "  * "
              , case n of
                  Nothing ->
                    "*" ++ t ++ "*"
                  Just n' ->
                    n'
              , "\n\n    "
              , "<a href=\""
              , uri
              , "\"><img src=\""
              , uri
              , "\" width=\"360\" alt=\""
              , fromMaybe t name
              , "\"/></a>"
              ]
          _ ->
            concat
              [
                "  * "
              , case n of
                  Nothing ->
                    ""
                  Just n' ->
                    n' ++ ": "
              , "["
              , t
              , "]("
              , uri
              , ")"
              ]
