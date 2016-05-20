module Data.Aviation.Casr.Logbook.Images (
  Images(..)
) where

import Data.Aviation.Casr.Logbook.Image
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.Aviation.Casr.Logbook.Printer.Markdown

newtype Images =
  Images
    [Image]
  deriving (Eq, Ord, Show)
    
instance Monoid Images where
  mempty =
    Images []
  Images i1 `mappend` Images i2 =
    Images (i1 `mappend` i2)

instance Markdown Images where
  markdown (Images i) =
    case i of
      [] ->
        []        
      _ ->
        "\n<div style=\"text-align: justify\">\n" ++ (i >>= \j -> markdown j ++ "\n") ++ "</div>"

instance Html Images where
  html (Images i) =
    case i of
      [] ->
        []        
      _ ->
        "<div style=\"text-align: justify\">" ++ (i >>= html) ++ "</div>"
