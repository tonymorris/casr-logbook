module Data.Aviation.Casr.Logbook.ExamEntry (
  ExamEntry(..)
) where

import Data.Aviation.Casr.Logbook.ARN
import Data.Aviation.Casr.Logbook.Date
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html

data ExamEntry =
  ExamEntry
    Date
    String -- name
    String -- delegate name
    String -- delegate rating
    ARN -- delegate ARN
    Int -- result
    Int -- maximum possible result
  deriving (Eq, Ord, Show)

instance Markdown ExamEntry where
  markdown (ExamEntry d n dn dr da r m) =
    concat [
      markdown d
    , "* Exam: **"
    , markdown n
    , "**\n"
    , "* Delegate Name: **"
    , markdown dn
    , " ("
    , markdown dr
    , ")**\n"
    , markdown da
    , "* Result: **`"
    , markdown r
    , "/"
    , markdown m
    , "`**"
    ]

instance Html ExamEntry where
  html (ExamEntry date n dn dr da r m) =
    concat
      [
        "<div class=\"examlog\">"
      , "<h3 class=\"exam\">"
      , html n
      , "</h3>"
      , "</div>"
      , "<ul>"
      , "<li class=\"date\">"
      , html date
      , "</li>"
      , "<li class=\"delegate\">"
      , html dn
      , " ("
      , html dr
      , ")"
      , "</li>"
      , "<li class=\"delegatearn\">"
      , concat
        [
          "<span class=\"heading delegatearnheading\">"
        , "Delegate ARN"
        , "</span>"
        , ": "
        , "<span class=\"info delegatearninfo\">"
        , html da
        , "</span>"
        ]
      , "</li>"
      , "<li class=\"examresult\">"
      , concat
        [
          "<span class=\"heading resultheading\">"
        , "Result"
        , "</span>"
        , ": "
        , "<span class=\"info resultinfo\">"
        , html r
        , "/"
        , html m
        , "</span>"
        ]
      , "</li>"
      , "</ul>"
      ]
