{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.Casr.Logbook.Meta.Html(
    htmlAircraftUsageExpense
  , htmlAircraftLandingExpense
  , htmlAircraftFlightExpense
  , htmlSimulatorFlightExpense
  , htmlExamExpense
  , htmlBriefingExpense
  , htmlVisualisation
  , strImageType
  , htmlImageSource
  , htmlImage
  , strTrackLogType
  , htmlTrackLogSource
  , htmlTrackLog
  , strVideoType
  , htmlVideoSource
  , htmlVideo
  , htmlTrackLogs
  , htmlVisualisations
  , htmlImages
  , htmlVideos
  , htmlAircraftFlightExpenses
  , htmlAircraftFlightMeta
  , htmlSimulatorFlightMeta
  , htmlExamMeta
  , htmlBriefingMeta
  , htmlLogbookDocumentMeta
  , showCentsAsDollars
  , showThousandCentsAsDollars
  , showHundredCentsAsDollars
  , whenEmpty  
) where

import Control.Category((.), id)
import Control.Monad(when)
import Data.Aviation.Casr.Logbook.Types(
    AircraftFlight
  , SimulatorFlight
  , Briefing
  , Exam
  , Logbook
  )
import Data.Aviation.Casr.Logbook.Html.Html(htmlLogbookDocument)
import Data.Aviation.Casr.Logbook.Meta(
    AircraftFlightExpense(ExpenseAircraftUsage, ExpenseAircraftLanding)
  , AircraftFlightMeta(AircraftFlightMeta)
  , AircraftLandingExpense(AircraftLandingExpense)
  , AircraftUsageExpense(AircraftUsageExpense)
  , BriefingExpense(BriefingExpense)
  , BriefingMeta(BriefingMeta)
  , ExamExpense(ExamExpense)
  , ExamMeta(ExamMeta)
  , Image(Image)
  , ImageType(Jpg, Png, Gif)
  , Passenger(Passenger)
  , SimulatorFlightExpense(SimulatorFlightExpense)
  , SimulatorFlightMeta(SimulatorFlightMeta)
  , TrackLog(TrackLog)
  , TrackLogType(Gpx, Kml, Kmz, ImageTrackLog)
  , Video(Video)
  , VideoType(YouTube, Vimeo, Bambuser)
  , Visualisation(Doarama)
  , linkVideoType
  , aircraftUsageCost
  , simulatorFlightCost
  , briefingCost
  ) 
import Data.Bool(not)
import Data.Foldable(mapM_, null)
import Data.Function(($))
import Data.Int(Int)
import Data.List(reverse)
import Data.Maybe(Maybe, maybe, fromMaybe)
import Data.Monoid(Monoid, (<>), mempty)
import Data.Ord((<))
import Data.String(String, fromString)
import qualified Data.Text as Text(pack)
import Lucid(
    class_
  , span_
  , a_
  , div_
  , href_
  , src_
  , ul_
  , li_
  , width_
  , img_
  , alt_
  , br_
  , style_
  , Html
  )
import Prelude(show, (*), abs)

htmlAircraftUsageExpense ::
  AircraftFlight
  -> AircraftUsageExpense
  -> Html ()
htmlAircraftUsageExpense fl e@(AircraftUsageExpense perhour name) =
  span_ [class_ "aircraftusageexpense"] $
    do  span_ [class_ "aircraftusageexpensecost"] . fromString . ('$':) . showThousandCentsAsDollars $ aircraftUsageCost fl e
        span_ [class_ "aircraftusageexpensephrase"] " at "
        span_ [class_ "aircraftusageexpenseperhour"] . fromString . ('$':) . showCentsAsDollars $ perhour
        span_ [class_ "aircraftusageexpensephrase"] " per hour"
        when (not . null $ name) . span_ [class_ "aircraftusageexpensename"] $
          do  " ("
              fromString name
              ")"

htmlAircraftLandingExpense ::
  AircraftFlight
  -> AircraftLandingExpense
  -> Html ()
htmlAircraftLandingExpense _ (AircraftLandingExpense amount name) =
  span_ [class_ "aircraftlandingexpense"] $
    do  span_ [class_ "aircraftlandingexpensecost"] . fromString . ('$':) . showThousandCentsAsDollars $ (amount * 10)
        when (not . null $ name) . span_ [class_ "aircraftlandingexpensename"] $
          do  " ("
              fromString name
              ")"

htmlAircraftFlightExpense ::
  AircraftFlight
  -> AircraftFlightExpense
  -> Html ()
htmlAircraftFlightExpense fl (ExpenseAircraftUsage e) =
  htmlAircraftUsageExpense fl e
htmlAircraftFlightExpense fl (ExpenseAircraftLanding e) =
  htmlAircraftLandingExpense fl e

htmlSimulatorFlightExpense ::
  SimulatorFlight
  -> SimulatorFlightExpense
  -> Html ()
htmlSimulatorFlightExpense sf e@(SimulatorFlightExpense perhour name) =
  span_ [class_ "simulatorflightexpense"] $
    do  span_ [class_ "simulatorflightcost"] . fromString . ('$':) . showThousandCentsAsDollars $ simulatorFlightCost sf e
        span_ [class_ "simulatorflightexpensephrase"] " at "
        span_ [class_ "simulatorflightexpenseperhour"] . fromString . ('$':) . showCentsAsDollars $ perhour
        span_ [class_ "simulatorflightexpensephrase"] " per hour"
        when (not . null $ name) . span_ [class_ "simulatorflightexpensename"] $
          do  " ("
              fromString name
              ")"

htmlExamExpense ::
  Exam
  -> ExamExpense
  -> Html ()
htmlExamExpense _ (ExamExpense amount name) =
  span_ [class_ "examexpense"] $
    do  span_ [class_ "examexpensecost"] . fromString . ('$':) . showThousandCentsAsDollars $ (amount * 10)
        when (not . null $ name) . span_ [class_ "examexpensename"] $
          do  " ("
              fromString name
              ")"

htmlBriefingExpense ::
  Briefing
  -> BriefingExpense
  -> Html ()
htmlBriefingExpense br e@(BriefingExpense perhour name) =
  span_ [class_ "briefingexpense"] $
    do  span_ [class_ "briefingexpensecost"] . fromString . ('$':) . showThousandCentsAsDollars $ briefingCost br e
        span_ [class_ "briefingexpensephrase"] " at "
        span_ [class_ "briefingexpenseperhour"] . fromString . ('$':) . showCentsAsDollars $ perhour
        span_ [class_ "briefingexpensephrase"] " per hour"
        when (not . null $ name) . span_ [class_ "briefingexpensename"] $
          do  " ("
              fromString name
              ")"

htmlVisualisation ::
  AircraftFlight
  -> Visualisation
  -> Html ()
htmlVisualisation _ (Doarama i _ n) =
  let n' = fromMaybe "doarama.com" n
  in  do  a_ [href_ ("http://doarama.com/view/" <> Text.pack i)] $ 
            span_ [class_ "Visualisation_name"] (fromString n')
          -- p_ (iframe_ [src_ ("http://www.doarama.com/embed?k=" <> Text.pack e), width_ "560", height_ "315", termWith -- "allowfullscreen" [] "allowfullscreen"] "")

strImageType ::
  ImageType
  -> String
strImageType Jpg =
  "jpg"
strImageType Png =
  "png"
strImageType Gif =
  "gif"

htmlImageSource ::
  AircraftFlight
  -> Maybe String
  -> Html ()
htmlImageSource _ =
  maybe mempty (\s' -> span_ [] (fromString ("Image source: " <> s')))

htmlImage ::
  AircraftFlight
  -> Image
  -> Html ()
htmlImage fl (Image u t s n) =
  let u' = fromString u      
      n' = fromMaybe ("Image (" <> strImageType t <> ")") n
  in  do  a_ [href_ u'] $
            img_ [src_ u', width_ "120", alt_ (Text.pack n')]
          htmlImageSource fl s

strTrackLogType ::
  TrackLogType
  -> String
strTrackLogType Gpx =
  "gpx"
strTrackLogType Kml =
  "kml"
strTrackLogType Kmz =
  "kmz"
strTrackLogType (ImageTrackLog i) =
  strImageType i

htmlTrackLogSource ::
  AircraftFlight
  -> Maybe String
  -> Html ()
htmlTrackLogSource _ =
  maybe "" (\q -> span_ [] (fromString (" from " <> q)))

htmlTrackLog ::
  AircraftFlight
  -> TrackLog
  -> Html ()
htmlTrackLog fl (TrackLog u t s n) =
  let u' = fromString u
      n' = fromMaybe (strTrackLogType t) n
      o = do  fromString n'
              htmlTrackLogSource fl s
  in  do  a_ [href_ u'] o
          case t of 
            ImageTrackLog _ ->
              do  br_ []
                  a_ [href_ u'] $
                    img_ [src_ u', width_ "360", alt_ (fromString n')]
            _ ->
              mempty

strVideoType ::
  VideoType
  -> String
strVideoType YouTube =
  "youtube"
strVideoType Vimeo =
  "vimeo"
strVideoType Bambuser =
  "bambuser"

htmlVideoSource ::
  AircraftFlight
  -> Maybe String
  -> Html ()
htmlVideoSource _ s =
  maybe mempty (\q -> span_ [] (fromString (" from " <> q))) s

htmlVideo ::
  AircraftFlight
  -> Video
  -> Html ()
htmlVideo fl (Video u t s n) =
  let n' = fromMaybe ("Video (" <> strVideoType t <> ")") n
  in  do  a_ [href_ (fromString (linkVideoType t u))] (fromString n')
          htmlVideoSource fl s

htmlTrackLogs ::
  AircraftFlight
  -> [TrackLog]
  -> Html ()
htmlTrackLogs fl x =
  whenEmpty (\q -> div_ [class_ "tracklogs"] $
    do  span_ [class_ "tracklogsheader"] "Track Logs"
        ul_ [] $
          mapM_ (li_ [class_ "tracklog"] . htmlTrackLog fl) q) x

htmlVisualisations ::
  AircraftFlight
  -> [Visualisation]
  -> Html ()
htmlVisualisations fl x =
  whenEmpty (\q -> div_ [class_ "visualisations"] $
    do  span_ [class_ "visualisationsheader"] "Visualisations"
        ul_ [] $
          mapM_ (li_ [class_ "visualisation"] . htmlVisualisation fl) q) x

htmlImages ::
  AircraftFlight
  -> [Image]
  -> Html ()
htmlImages fl x =
  whenEmpty (\q -> div_ [class_ "tracklogs"] $
    do  span_ [class_ "imagesheader"] "Images"
        div_ [style_ "text-align: justify"] $ 
          mapM_ (htmlImage fl) q) x

htmlVideos ::
  AircraftFlight
  -> [Video]
  -> Html ()
htmlVideos fl x =
  whenEmpty (\q -> div_ [class_ "videos"] $
    do  span_ [class_ "videosheader"] "Videos"
        ul_ [] $
          mapM_ (li_ [class_ "video"] . htmlVideo fl) q) x

htmlAircraftFlightExpenses ::
  AircraftFlight
  -> [AircraftFlightExpense]
  -> Html ()
htmlAircraftFlightExpenses fl x =
  whenEmpty (\q -> div_ [class_ "aircraftflightexpenses"] $
    do  span_ [class_ "aircraftflightexpensesheader"] "Aircraft Flight Expenses"
        ul_ [] $
          mapM_ (li_ [class_ "aircraftflightexpense"] . htmlAircraftFlightExpense fl) q) x

htmlAircraftFlightPassenger ::
  AircraftFlight
  -> Passenger
  -> Html ()
htmlAircraftFlightPassenger _ (Passenger p) =
  do span_ [class_ "aircraftflightpassenger"] (fromString p)

htmlAircraftFlightPax ::
  AircraftFlight
  -> [Passenger]
  -> Html ()
htmlAircraftFlightPax fl x =
  whenEmpty (\q -> div_ [class_ "aircraftflightpax"] $
    do  span_ [class_ "aircraftflightpaxheader"] "PAX"
        ul_ [] $
          mapM_ (li_ [class_ "aircraftflightpassenger"] . htmlAircraftFlightPassenger fl) q) x

htmlAircraftFlightMeta ::
  AircraftFlight
  -> AircraftFlightMeta
  -> Html ()
htmlAircraftFlightMeta fl (AircraftFlightMeta tls vls ims vds exs pax) =
  div_ $ 
    do  htmlTrackLogs fl tls
        htmlVisualisations fl vls
        htmlImages fl ims
        htmlVideos fl vds
        htmlAircraftFlightExpenses fl exs
        htmlAircraftFlightPax fl pax

htmlSimulatorFlightMeta ::
  SimulatorFlight
  -> SimulatorFlightMeta
  -> Html ()
htmlSimulatorFlightMeta fl (SimulatorFlightMeta s) =
  whenEmpty (\q -> div_ [class_ "simulatormeta"] $
    do  span_ [class_ "simulatorheader"] "Expenses"
        ul_ [] $
          mapM_ (li_ [class_ "expense"] . htmlSimulatorFlightExpense fl) q) s  

htmlExamMeta ::
  Exam
  -> ExamMeta
  -> Html ()
htmlExamMeta e (ExamMeta s) =
  whenEmpty (\q -> div_ [class_ "exammeta"] $
    do  span_ [class_ "exammetaheader"] "Expenses"
        ul_ [] $
          mapM_ (li_ [class_ "expense"] . htmlExamExpense e) q) s

htmlBriefingMeta ::
  Briefing
  -> BriefingMeta
  -> Html ()
htmlBriefingMeta b (BriefingMeta s) =
  whenEmpty (\q -> div_ [class_ "briefingmeta"] $
    do  span_ [class_ "briefingmetaheader"] "Expenses"
        ul_ [] $
          mapM_ (li_ [class_ "expense"] . htmlBriefingExpense b) q) s

htmlLogbookDocumentMeta ::
  Html ()
  -> Logbook AircraftFlightMeta SimulatorFlightMeta ExamMeta BriefingMeta
  -> Html ()
htmlLogbookDocumentMeta =
  htmlLogbookDocument htmlAircraftFlightMeta htmlSimulatorFlightMeta htmlExamMeta htmlBriefingMeta

----

showCentsAsDollars ::
  Int
  -> String
showCentsAsDollars n =
  let pos ::
        String
        -> String
      pos [] =
        []
      pos [x] =
        "0.0" <> [x]
      pos [x, y] =
        "0." <> [y, x]
      pos (x:y:z) =
        reverse z <> "." <> [y, x]
  in  (if n < 0 then ('-':) else id) . pos . reverse . show . abs $ n

showThousandCentsAsDollars ::
  Int
  -> String
showThousandCentsAsDollars n =
  let pos ::
        String
        -> String
      pos [] =
        []
      pos [x] =
        [x] <> "0.0"
      pos [x, y] =
        [x, y] <> ".0"
      pos [x, y, z] =
        [x, y, z] <> ".0"
      pos (x:y:z:r) =
        [x, y, z] <> "." <> r
      drop0 [] =
        []
      drop0 ('0':r) =
        r
      drop0 w =
        w
  in  (if n < 0 then ('-':) else id) . reverse . drop0 . pos . reverse . show . abs $ n

showHundredCentsAsDollars ::
  Int
  -> String
showHundredCentsAsDollars n =
  let pos ::
        String
        -> String
      pos [] =
        []
      pos [x] =
        [x] <> "0.0"
      pos [x, y] =
        [x, y] <> ".0"
      pos (x:y:r) =
        [x, y] <> "." <> r
      drop0 [] =
        []
      drop0 ('0':r) =
        r
      drop0 w =
        w
  in  (if n < 0 then ('-':) else id) . reverse . drop0 . pos . reverse . show . abs $ n

whenEmpty ::
  Monoid a =>
  ([t] -> a)
  -> [t]
  -> a
whenEmpty _ [] =
  mempty
whenEmpty f x =
  f x
