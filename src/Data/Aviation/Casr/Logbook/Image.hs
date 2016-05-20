module Data.Aviation.Casr.Logbook.Image (
  Image(..)  
) where

import Data.Aviation.Casr.Logbook.ImageType
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Maybe

data Image =
  Image
    String -- uri
    (Maybe String) -- name
    ImageType
  deriving (Eq, Ord, Show)

instance Markdown Image where
  markdown (Image uri name itype) =
    let t = markdown itype
        n = fromMaybe ("Image (" ++ t ++ ")") name
    in  concat
          [
            "<a href=\""
          , uri
          , "\"><img src=\""
          , uri
          , "\" width=\"120\" alt=\""
          , n
          , "\"></a>"
          ]

instance Html Image where
  html (Image uri name itype) =
    let t = html itype
        n = fromMaybe ("Image (" ++ t ++ ")") name
    in  concat
          [
            "<a href=\""
          , uri
          , "\"><img src=\""
          , uri
          , "\" width=\"120\" alt=\""
          , n
          , "\"></a>"
          ]
