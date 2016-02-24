module Data.Aviation.Casr.Logbook.Printer.Markdown (
  Markdown(..)
, printMarkdown
, writeMarkdownFile
) where

class Markdown s where
  markdown ::
    s
    -> String

printMarkdown ::
  Markdown s =>
  s
  -> IO ()
printMarkdown =
  putStrLn . markdown

writeMarkdownFile ::
  Markdown s =>
  FilePath
  -> s
  -> IO ()
writeMarkdownFile p =
  writeFile p . markdown
  
