import Text.Pandoc

-- Getting the contents
import Control.Monad(forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension)

-- Access to the command line commands, Want to try and do it the haskell way
--import System.Cmd
--import System.Exit

getMarkdownPreset :: IO [FilePath]
--;;;;;;Preset Markdown
getMarkdownPreset = getMarkdown "/Users/james/Dropbox/Projects/Site/Studio/In/"

getMarkdown :: FilePath -> IO [FilePath]
--Finds all the files in a directory and filters out the markdown files
getMarkdown topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".","..",".DS_Store"]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getMarkdown path
      else return [path]
  let mdPaths = filter ((".markdown" ==)  . takeExtension)  (concat paths)
  return mdPaths

convertMdtoHtml :: IO ()
--Reads file for now using Bitcoin as a test to an html string
convertMdtoHtml = do
  contents <- readFile "/Users/james/Dropbox/Projects/Site/Studio/In/2012/Bitcoin/index.markdown"
  let pandoc = readMarkdown def contents
  let html = writeHtmlString siteOptions pandoc
  putStrLn html

siteOptions :: WriterOptions
--Site Options with all default except following
siteOptions = def { writerStandalone = True, writerTemplate = template }

template :: String
template = "<!DOCTYPE html>\n<head>\n <meta charset=\"utf-8\">\n  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge,chrome=1\">\n  <title>$title$</title>\n  <meta name=\"description\" content=\"\">\n  <meta name=\"viewport\" content=\"width=device-width\">\n  <link rel=\"stylesheet\" href=\"normalize.css\">\n  <link rel=\"stylesheet\" href=\"main.css\" />\n</head>\n<body>\n  <header>\n    <a class=\"logo\" href=\"/\"></a> \n  </header>\n  <article>\n    $body$ \n  </article>\n</body>\n</html>" 


{-
  Copy Folder to new location
  Convert markdown to html
  Create main page

  Personal Blog Static Site Generator

  Testing
    Add new fonts
    Have external image site ready
-}


