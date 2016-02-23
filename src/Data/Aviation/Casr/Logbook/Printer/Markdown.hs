module Data.Aviation.Casr.Logbook.Printer.Markdown (
  Markdown(..)
, printMarkdownFile
, writeMarkdownFile
) where

class Markdown s where
  markdown ::
    s
    -> String

printMarkdownFile ::
  Markdown s =>
  s
  -> IO ()
printMarkdownFile =
  putStrLn . markdown

writeMarkdownFile ::
  Markdown s =>
  FilePath
  -> s
  -> IO ()
writeMarkdownFile p =
  writeFile p . markdown
  