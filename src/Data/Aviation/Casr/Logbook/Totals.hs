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
import Data.Aviation.Casr.Logbook.FlightLogEntry
import Data.Aviation.Casr.Logbook.FlightLogEntries
import Data.Aviation.Casr.Logbook.Hours
import Data.Aviation.Casr.Logbook.PoB
import Data.Aviation.Casr.Logbook.PiC
import Data.Aviation.Casr.Logbook.Printer.Markdown
import Data.Foldable(foldl')
import Data.Map(Map)
import qualified Data.Map as Map

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
      (intype1 `mappend` intype2)
      (inreg1 `mappend` inreg2)
      (singleengine1 `mappend` singleengine2)
      (multiengine1 `mappend` multiengine2)
      (day1 `mappend` day2)
      (night1 `mappend` night2)
      (daynight1 `mappend` daynight2)
      (pic1 `mappend` pic2)

instance Markdown Totals where
  markdown (Totals total dualhours solohours intype inreg singleengine multiengine day night daynight pic) =
    let displayHours (Hours f p) =
          show f ++ "." ++ show p
        displayPoint x h =
          "* " ++ x ++ ": **`" ++ displayHours h ++ "`**\n"
        displayMap x m =
          "* " ++ x ++ "\n" ++ Map.foldrWithKey (\k h s -> "  * " ++ k ++ ": **`" ++ displayHours h ++ "`**\n" ++ s) "" m
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
 FlightLogEntry
 -> Totals
singleTotals (FlightLogEntry _ _ _ (Aircraft atype areg aeng) hours (PoB pob) _ dn (PiC pic) _ _ _ _) =
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
  FlightLogEntry
  -> Totals
  -> Totals
updateTotals =
  mappend . singleTotals

totals ::
  FlightLogEntries
  -> Totals
totals (FlightLogEntries e) =
  foldl' (flip updateTotals) zeroTotals e
