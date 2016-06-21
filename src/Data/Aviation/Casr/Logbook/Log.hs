module Data.Aviation.Casr.Logbook.Log (
  Log(..)
) where

import Data.Aviation.Casr.Logbook.ARN
import Data.Aviation.Casr.Logbook.DOB
import Data.Aviation.Casr.Logbook.Entries
import Data.Aviation.Casr.Logbook.Name
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.Aviation.Casr.Logbook.Totals

data Log =
  Log
    Name
    DOB
    ARN
    Entries
  deriving (Eq, Ord, Show)
    
instance Markdown Log where
  markdown (Log (Name name') dob arn entries) =
    concat
      [
        "# Pilot Personal Log Book\n"
      , "### Civil Aviation Safety Regulation 1998 (61.345) [*austlii.edu.au*](http://www.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s61.345.html)\n\n"
      , "* "
      , markdown name'
      , "\n"
      , markdown dob
      , markdown arn
      , "\n----\n\n"
      , markdown (totals entries)
      , "\n----\n\n"
      , markdown entries
      ]
    
instance Html Log where
  html (Log name@(Name name') dob arn@(ARN arn') entries) =
    concat
      [
        "<!DOCTYPE HTML>"
      , "<html lang=\"en\">"
      , "<head>"
      , "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">"
      , "<title>Pilot Personal Log Book &mdash; "
      , html name'
      , " ("
      , html arn'
      , ")"
      , "</title>"
      , "<link href=\"https://fonts.googleapis.com/css?family=Inconsolata:400,700\" rel=\"stylesheet\" type=\"text/css\">"
      , "<link rel=\"stylesheet\" type=\"text/css\" href=\"casr-logbook.css\">"
      , "<link rel=\"alternate\" type=\"application/atom+xml\" href=\"/atom.xml\" title=\"Atom feed\">"
      , "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
      , "<script type=\"text/javascript\" src=\"https://raw.github.com/Mathapedia/LaTeX2HTML5/master/latex2html5.min.js\"></script>"
      , "</head>"
      , "<body class=\"casr-logbook\">"    
      , "<div id=\"title\" class=\"title\">"
      , "<h1>Pilot Personal Log Book</h1>"
      , "</div>"
      , "<div id=\"subtitle\" class=\"subtitle\">"
      , "<h2>Civil Aviation Safety Regulation 1998 (61.345) <span class=\"austlii\"><a href=\"http://www.austlii.edu.au/au/legis/cth/consol_reg/casr1998333/s61.345.html\">austlii.edu.au</a></span></h2>"
      , "</div>" 
      , "<div id=\"personal\" class=\"personal\">"     
      , "<ul>"
      , "<li id=\"name\">"
      , html name
      , "</li>"
      , "<li id=\"dob\">"
      , html dob
      , "</li>"
      , "<li id=\"arn\">"
      , html arn
      , "</li>"
      , "</ul>"            
      , "</div>"
      , "<hr>"
      , html (totals entries)
      , "<hr>"
      , "<div id=\"flightlogentries\" class=\"flightlogentries\">"
      , html entries
      , "</div> "
      , "</body>"
      , "</html>"
      ]
