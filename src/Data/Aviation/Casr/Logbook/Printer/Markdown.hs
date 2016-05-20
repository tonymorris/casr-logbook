module Data.Aviation.Casr.Logbook.Printer.Markdown (
  Markdown(..)
, printMarkdown
, writeMarkdownFile
) where

class Markdown s where
  markdown ::
    s
    -> String

instance Markdown Char where
  markdown =
    pure

instance Markdown Int where
  markdown =
    show

instance Markdown a => Markdown [a] where
  markdown =
    (=<<) markdown

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
  
