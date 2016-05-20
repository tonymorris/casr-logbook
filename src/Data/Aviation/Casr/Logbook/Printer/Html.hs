module Data.Aviation.Casr.Logbook.Printer.Html (
  Html(..)
, printHtml
, writeHtmlFile
) where

class Html s where
  html ::
    s
    -> String

instance Html Char where
  html '&' =
    "&amp;"
  html c =
    [c]
    
instance Html Int where
  html =
    show
    
instance Html a => Html [a] where
  html =
    (=<<) html

printHtml ::
  Html s =>
  s
  -> IO ()
printHtml =
  putStrLn . html

writeHtmlFile ::
  Html s =>
  FilePath
  -> s
  -> IO ()
writeHtmlFile p =
  writeFile p . html
  