module Data.Aviation.Casr.Logbook.Visualisation (
  Visualisation(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Html
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.VisualisationType
import Data.Maybe

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

instance Html Visualisation where
  html (Visualisation uri name vtype) =
    let t = html vtype
        n = fromMaybe t name
    in  concat
          [
            "<a href=\""
          , uri
          , "\">"
          , "<span class=\"visualisationheading\">"
          , html n
          , "</span>"
          , "</a>"
          , "<p>"
          , case vtype of
              Doarama e ->
                concat
                  [
                    "<iframe src=\"http://www.doarama.com/embed?k="
                  , e
                  , "\" width=\"560\" height=\"315\" allowfullscreen=\"allowfullscreen\">"
                  , "</iframe>"
                  ]
          ]
