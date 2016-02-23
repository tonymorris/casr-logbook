module Data.Aviation.Casr.Logbook.Video (
  Video(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.VideoType
import Data.Maybe

data Video =
  Video
    String -- uri
    (Maybe String) -- name
    VideoType
  deriving (Eq, Ord, Show)

instance Markdown Video where
  markdown (Video uri name vtype) =
    let t = markdown vtype
        n = fromMaybe ("Video (" ++ t ++ ")") name
    in  concat
          [
            "**"
          , n
          , ":** ["
          , t
          , "]("
          , uri
          , ")"
          ]
