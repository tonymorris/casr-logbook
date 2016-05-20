module Data.Aviation.Casr.Logbook.TrackLog (
  TrackLog(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
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
    in  concat $ 
          case ttype of 
            ImageLog _ ->              
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
                , "\"></a>"
                ]
            _ ->              
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

instance Html TrackLog where
  html (TrackLog uri name ttype) =
    let t = html ttype   
        n = fmap html name
    in  concat [
            "<div class=\"tracklog\">"
          , concat $ 
              case ttype of 
                ImageLog _ ->
                  [
                    fromMaybe (concat
                          [
                            "<a href=\""
                          , uri
                          , "\">"
                          , "<span class=\"tracklogtype\">"
                          , t
                          , "</span>"
                          , "</a>"
                          ]) n
                  , "<p>"
                  , "<a href=\""
                  , uri
                  , "\">"
                  , "<span class=\"tracklogimage\">"
                  , "<img src=\""
                  , uri
                  , "\" width=\"360\" alt=\""
                  , fromMaybe t n
                  , "\">"
                  , "</span>"
                  , "</a>"
                  , "</p>"
                  ]
                _ ->
                  [
                    case n of
                      Nothing ->
                        ""
                      Just n' ->
                        n' ++ ": "
                  , "<a href=\""
                  , uri
                  , "\">"
                  , "<span class=\"tracklogtype\">"
                  , t
                  , "</span>"
                  , "</a>"
                  ]
             , "</div>"
          ]
