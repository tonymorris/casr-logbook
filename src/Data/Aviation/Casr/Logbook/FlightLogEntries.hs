module Data.Aviation.Casr.Logbook.FlightLogEntries (
  FlightLogEntries(..)
) where

import Data.Aviation.Casr.Logbook.Aircraft
import Data.Aviation.Casr.Logbook.Date
import Data.Aviation.Casr.Logbook.FlightLogEntry
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.List

newtype FlightLogEntries =
  FlightLogEntries
    [FlightLogEntry]
  deriving (Eq, Ord, Show)

instance Monoid FlightLogEntries where
  mempty =
    FlightLogEntries []
  FlightLogEntries e1 `mappend` FlightLogEntries e2 =
    FlightLogEntries (e1 `mappend` e2)

instance Markdown FlightLogEntries where
  markdown (FlightLogEntries g) =
    intercalate "\n\n----\n\n" (fmap markdown g)

instance Html FlightLogEntries where
  html (FlightLogEntries g) =
    (\(i, h@(FlightLogEntry _ (Date d) (Aircraft _ reg _) _ _ _ _ _ _ _ _ _)) ->
      let divid = concat
                    [
                      d
                    , "_"
                    , reg
                    , "_"
                    , show (i :: Int)
                    ]
      in  concat
            [
              "<div id=\""
            , divid
            , "\" class=\"flightlogentry\">"
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
            ]) =<< zip [0..] g
