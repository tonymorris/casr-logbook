module Data.Aviation.Casr.Logbook.Entries (
  Entries(..)
, flightEntries
) where

import Data.Aviation.Casr.Logbook.Aircraft
import Data.Aviation.Casr.Logbook.Date
import Data.Aviation.Casr.Logbook.Entry
import Data.Aviation.Casr.Logbook.ExamEntry
import Data.Aviation.Casr.Logbook.FlightEntry
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.List

newtype Entries =
  Entries
    [Entry]
  deriving (Eq, Ord, Show)

instance Monoid Entries where
  mempty =
    Entries []
  Entries e1 `mappend` Entries e2 =
    Entries (e1 `mappend` e2)

instance Markdown Entries where
  markdown (Entries g) =
    intercalate "\n\n----\n\n" (fmap markdown g)

instance Html Entries where
  html (Entries g) =
    (\(i, h) -> let nospace =
                      map (\c -> case c of
                                   ' ' ->
                                     '_'
                                   _ ->
                                     c)
                    divid =
                      case h of
                        EntryFlight (FlightEntry _ (Date d) (Aircraft _ reg _) _ _ _ _ _ _ _ _ _) ->
                          concat
                            [
                              nospace d
                            , "_"
                            , nospace reg
                            , "_"
                            , show (i :: Int)
                            ]
                        EntryExam (ExamEntry (Date d) n _ _ _ _ _) ->
                            concat
                              [
                                nospace d
                              , "_"
                              , nospace n
                              ]
                    divclass =
                      case h of
                        EntryFlight _ ->
                          "flightlogentry"
                        EntryExam _ ->
                          "examentry"
                in  concat
                      [
                        "<div id=\""
                      , divid
                      , "\" class=\""
                      , divclass
                      , "\">"
                      , "<div class=\"hreflink\">"
                      , "<a href=\"#"
                      , divid
                      , "\">"
                      , "§"
                      , "</a>"
                      , "</div>"
                      , html h
                      , "</div>"
                      , "<hr>"
                      ]
                      ) =<< zip [0..] g
    
flightEntries ::
 Entries
 -> [FlightEntry]
flightEntries (Entries e) =
  foldr (\x z -> case x of EntryFlight f -> f:z
                           EntryExam _ -> z) [] e
