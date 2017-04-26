{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Casr.Logbook.Reports.Meta.Html(
  htmlExpenseReport
, htmlReports
) where

import Control.Category((.))
import Data.Aviation.Casr.Logbook.Reports(getFlightTimeReport, getSimulatorTimeReport)
import Data.Aviation.Casr.Logbook.Reports.Html(htmlFlightTimeReport, htmlSimulatorTimeReport, htmlTakeOffLanding90, takeoffslandings90)
import Data.Aviation.Casr.Logbook.Reports.Meta(ExpenseReport(ExpenseReport), logbookExpenseReport)
import Data.Aviation.Casr.Logbook.Meta(AircraftFlightMeta, SimulatorFlightMeta, ExamMeta, BriefingMeta)
import Data.Aviation.Casr.Logbook.Meta.Html(showThousandCentsAsDollars, showHundredCentsAsDollars)
import Data.Aviation.Casr.Logbook.Types(Logbook)
import Data.Function(($))
import Data.String(fromString)
import qualified Data.Text as Text(pack)
import Lucid(
    hr_
  , class_
  , ul_
  , li_
  , span_
  , h3_
  , div_
  , a_
  , id_
  , href_
  , Html
  )
import Prelude((+))

htmlExpenseReport ::
  Logbook AircraftFlightMeta SimulatorFlightMeta ExamMeta BriefingMeta
  -> ExpenseReport
  -> Html ()
htmlExpenseReport _ (ExpenseReport ag al b e s) =
  div_ [class_ "expensereport"] $
    do  a_ [id_ "RPT_Expense"] ""
        a_ [href_ (Text.pack ("#RPT_Expense"))] . span_ [class_ "entrytag"] $ "RPT"
        h3_ [class_ "expensereportname"] "Expense Report"     
        do  ul_ [] $
              do  li_ [] $
                    do  span_ [class_ "key"] "Aircraft: "
                        span_ [class_ "value"] . fromString . ('$':) . showThousandCentsAsDollars $ ag + al
                        ul_ [] $
                          do  li_ [] $
                                do  span_ [class_ "key"] "Usage: "
                                    span_ [class_ "value"] . fromString . ('$':) . showThousandCentsAsDollars $ ag
                              li_ [] $
                                do  span_ [class_ "key"] "Landing: "
                                    span_ [class_ "value"] . fromString . ('$':) . showHundredCentsAsDollars $ al
                  li_ [] $
                    do  span_ [class_ "key"] "Briefing: "
                        span_ [class_ "value"] . fromString . ('$':) . showThousandCentsAsDollars $ b
                  li_ [] $
                    do  span_ [class_ "key"] "Exam: "
                        span_ [class_ "value"] . fromString . ('$':) . showHundredCentsAsDollars $ e
                  li_ [] $
                    do  span_ [class_ "key"] "Simulator: "
                        span_ [class_ "value"] . fromString . ('$':) . showThousandCentsAsDollars $ s
                  li_ [] $
                    do  span_ [class_ "key"] "TOTAL: "
                        span_ [class_ "value"] . fromString . ('$':) . showThousandCentsAsDollars $ (ag + al + b + e + s)

htmlReports ::
  Logbook AircraftFlightMeta SimulatorFlightMeta ExamMeta BriefingMeta
  -> Html ()
htmlReports b =
  do  htmlFlightTimeReport b (getFlightTimeReport b)
      hr_ [] 
      htmlSimulatorTimeReport b (getSimulatorTimeReport b)
      hr_ [] 
      htmlTakeOffLanding90 b (takeoffslandings90 b)
      hr_ [] 
      htmlExpenseReport b (logbookExpenseReport b)
