module Data.Aviation.Casr.Logbook.Video (
  Video(..)
) where

import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
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
          , "https://www.youtube.com/watch?v="
          , uri
          , ")"
          ]

instance Html Video where
  html (Video uri name vtype) =
    let t = html vtype
        n = fromMaybe ("Video (" ++ t ++ ")") name
    in  concat
          [
            "<div>"
          , case vtype of
              YouTube ->
                concat
                  [
                    "<a href=\"https://www.youtube.com/watch?v="
                  , uri
                  , "\">"
                  , "<span class=\"videoname\">"
                  , html n
                  , "</span>"
                  , "</a>"
                  ]
              Bambuser ->
                concat
                  [
                    "<a href=\"https://bambuser.com/v/"
                  , uri
                  , "\">"
                  , "<span class=\"videoname\">"
                  , html n
                  , "</span>"
                  , "</a>"
                  ]
              Vimeo ->
                concat
                  [
                    "<a href=\"https://vimeo.com/"
                  , uri
                  , "\">"
                  , "<span class=\"videoname\">"
                  , html n
                  , "</span>"
                  , "</a>"
                   ]
          , "</div>"
          , "<p>"
          , case vtype of
              YouTube ->
                concat
                  [  
                    "<iframe width=\"560\" height=\"315\" allowfullscreen=\"allowfullscreen\" src=\"http://www.youtube.com/embed/"
                  , uri
                  , "?autohide=1&amp;cc_load_policy=1&amp;color=white&amp;controls=1&amp;disablekb=0&amp;fs=1&amp;iv_load_policy=0&amp;loop=0&amp;modestbranding=1&amp;rel=0&amp;showinfo=0\">"
                  , "</iframe>"                                  
                  ]
              Bambuser ->
                concat
                  [                 
                    "<iframe width=\"560\" height=\"315\" allowfullscreen=\"allowfullscreen\" src=\"https://embed.bambuser.com/broadcast/"
                  , uri
                  , "?chat=1&amp;mute=0"
                  , "\">"
                  , "</iframe>"                   
                  ]
              Vimeo ->
                concat
                  [ 
                    "<iframe width=\"560\" height=\"315\" allowfullscreen=\"allowfullscreen\" src=\"https://player.vimeo.com/video/"
                  , uri
                  , "\">"
                  , "</iframe>"                                  
                  ]
            , "</p>"
          ]
