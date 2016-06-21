module Data.Aviation.Casr.Logbook.Totals (
  Totals(..)
, zeroTotals
, singleTotals
, updateTotals
, totals
) where

import Data.Aviation.Casr.Logbook.Aircraft
import Data.Aviation.Casr.Logbook.DayNight
import Data.Aviation.Casr.Logbook.Engine
import Data.Aviation.Casr.Logbook.Entries
import Data.Aviation.Casr.Logbook.FlightEntry
import Data.Aviation.Casr.Logbook.Hours
import Data.Aviation.Casr.Logbook.PoB
import Data.Aviation.Casr.Logbook.PiC
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Aviation.Casr.Logbook.Printer.Html
import Data.Foldable(foldl')
import Data.Map(Map)
import qualified Data.Map as Map
import Text.Printf

data Totals =
  Totals
    Hours -- total
    Hours -- dual
    Hours -- solo
    (Map String Hours) -- type
    (Map String Hours) -- aircraft
    Hours -- single-engine
    Hours -- multi-engine
    Hours -- day
    Hours -- night
    Hours -- day & night
    (Map String Hours) -- pilot in command
  deriving (Eq, Ord, Show)

instance Monoid Totals where
  mempty =
    zeroTotals
  Totals total1 dualhours1 solohours1 intype1 inreg1 singleengine1 multiengine1 day1 night1 daynight1 pic1 `mappend` Totals total2 dualhours2 solohours2 intype2 inreg2 singleengine2 multiengine2 day2 night2 daynight2 pic2 =
    Totals
      (total1 `mappend` total2)
      (dualhours1 `mappend` dualhours2)
      (solohours1 `mappend` solohours2)
      (Map.unionWith mappend intype1 intype2)
      (Map.unionWith mappend inreg1 inreg2)
      (singleengine1 `mappend` singleengine2)
      (multiengine1 `mappend` multiengine2)
      (day1 `mappend` day2)
      (night1 `mappend` night2)
      (daynight1 `mappend` daynight2)
      (Map.unionWith mappend pic1 pic2)

instance Markdown Totals where
  markdown (Totals total dualhours solohours intype inreg singleengine multiengine day night daynight pic) =
    let displayHours (Hours f p) =
          show f ++ "." ++ show p
        displayPoint x h =
          "* " ++ x ++ ": **`" ++ displayHours h ++ "`**\n"
        displayMap x m =
          "* " ++ x ++ "\n" ++ Map.foldrWithKey (\k h s -> "  * " ++ k ++ ": **`" ++ displayHours h ++ " (" ++ printf "%.2f" (fractionalHours h / fractionalHours total * 100 :: Double) ++ "%)`**\n" ++ s) "" m
    in  concat
          [
            "##### Summary\n"
          , displayPoint "Total Hours" total
          , displayPoint "Dual Hours" dualhours
          , displayPoint "Solo Hours" solohours
          , displayMap "Hours in type" intype
          , displayMap "Hours in registration" inreg
          , displayPoint "Single-engine Hours" singleengine
          , displayPoint "Multi-engine Hours" multiengine
          , displayPoint "Day Hours" day
          , displayPoint "Night Hours" night
          , displayPoint "Day & Night Hours" daynight
          , displayMap "Hours with PiC" pic            
          ]

instance Html Totals where
  html (Totals total dualhours solohours intype inreg singleengine multiengine day night daynight pic) =
    let displayHours (Hours f p) =
          show f ++ "." ++ show p
        displayPoint x h q =
          concat
            [
              "<li class=\"summarypoint\">"
            , "<span id=\""
            , q
            , "\" class=\"heading summarypointheading summarypointheading"
            , q
            , "\">"
            , x            
            , "</span>: <span class=\"summarypoint summarypoint"
            , q
            , "\">"            
            , displayHours h
            , "</span></li>"
            ]
        displayMap x m q =
          concat
            [
              "<li class=\"summarypoint summarypoint"
            , q
            , "\">"
            , "<span id=\""
            , q
            , "\" class=\"heading summarypointheading summarypointheading"
            , q
            , "\">"
            , x            
            , "</span>: <div class=\"summarypoint summarypoint"
            , q
            , "\"><ul>"
            , Map.foldrWithKey (\k h s -> concat [
                                                   "<li class=\"subsummarypoint subsummarypoint"
                                                 , q
                                                 , "\">"
                                                 , "<span class=\"heading subsummarypointheading subsummarypointheading"
                                                 , q
                                                 , "\">"
                                                 , k
                                                 , "</span>: <span class=\"subsummarypointhours subsummarypointhours"
                                                 , q
                                                 , "\">"
                                                 , displayHours h
                                                 , "</span> <span class=\"subsummarypointpercentage subsummarypointpercentage"
                                                 , q
                                                 , "\">("
                                                 , printf "%.2f" (fractionalHours h / fractionalHours total * 100 :: Double)
                                                 , "%)</span>"
                                                 , "</li>"
                                                 , s
                                                 ]) "" m
            , "</ul></div></li>"
            ]        
    in  concat
          [
            "<div id=\"summary\" class=\"totals\">"
          , "<div class=\"hreflink\">"
          , "<a href=\"#summary\">§</a>"
          , "</div>"
          , "<h5>Summary</h5>"
          , "<ul>"
          , displayPoint "Total Hours" total "totalhours"
          , displayPoint "Dual Hours" dualhours "dualhours"
          , displayPoint "Solo Hours" solohours "solohours"
          , displayMap "Hours in type" intype "hoursintype"
          , displayMap "Hours in registration" inreg "hoursinregistration"
          , displayPoint "Single-engine Hours" singleengine "singleenginehours"
          , displayPoint "Multi-engine Hours" multiengine "multienginehours"
          , displayPoint "Day Hours" day "dayhours"
          , displayPoint "Night Hours" night "nighthours"
          , displayPoint "Day &amp; Night Hours" daynight "daynighthours"
          , displayMap "Hours with PiC" pic "pichours"           
          , "</ul>"
          , "</div>"
          ]

zeroTotals ::
  Totals
zeroTotals =
  Totals
    zeroHours
    zeroHours
    zeroHours
    Map.empty
    Map.empty
    zeroHours
    zeroHours
    zeroHours
    zeroHours
    zeroHours
    Map.empty

singleTotals ::
 FlightEntry
 -> Totals
singleTotals (FlightEntry _ _ (Aircraft atype areg aeng) hours (PoB pob) _ dn (PiC pic) _ _ _ _) =
  Totals
    hours
    (
      case pob of
        2 -> hours
        _ -> zeroHours
    )
    (
      case pob of
        1 -> hours
        _ -> zeroHours
    )
    (Map.singleton atype hours)
    (Map.singleton areg hours)
    (
      case aeng of
        Single -> hours
        _ -> zeroHours
    )
    (
      case aeng of
        Multi -> hours
        _ -> zeroHours
    )
    (
      case dn of
        Day -> hours
        _ -> zeroHours
    )
    (
      case dn of
        Night -> hours
        _ -> zeroHours
    )
    (
      case dn of
        Night -> hours
        _ -> zeroHours
    )
    (Map.singleton pic hours)

updateTotals ::
  FlightEntry
  -> Totals
  -> Totals
updateTotals =
  mappend . singleTotals

totals ::
  Entries
  -> Totals
totals =
  foldl' (flip updateTotals) zeroTotals . flightEntries
