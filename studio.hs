import Text.Pandoc

-- Getting the contents
import Control.Monad(forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension)
import System.IO(writeFile)
-- Access to the command line commands, Want to try and do it the haskell way, 
-- a lot easier this way for now
import System.Cmd
import System.Exit

--Copies a directory to a destination from system command, somewhat dangerous (constants added)
--"Out" folder must exist
copyDir :: IO ExitCode
copyDir = system $ "cp -r ~/Dropbox/Projects/Site/Studio/In/* ~/Dropbox/Projects/Site/Studio/Out/"

--;;;;;;Preset Markdown
getMarkdownPreset :: IO [FilePath]
getMarkdownPreset = getMarkdown "/Users/james/Dropbox/Projects/Site/Studio/Out/"

--Finds all the files in a directory and filters out the markdown files
getMarkdown :: FilePath -> IO [FilePath]
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

--Takes a markdown file and writes a html file, same name in same dir
convertMdtoHtml :: FilePath -> IO () 
convertMdtoHtml file = do
  contents <- readFile file 
  let pandoc = readMarkdown def contents
  template <- readFile "/Users/james/Dropbox/Projects/Site/Studio/In/template.html" 
  let html = writeHtmlString (siteOptions template) pandoc
  writeFile (file ++ ".html") html

convertArticles :: IO ()
convertArticles = undefined

--Site Options with all default except following
siteOptions :: String -> WriterOptions
siteOptions template = def { writerStandalone = True, writerTemplate = template }

{-
  Copy Folder to new location
  Convert markdown to html
  Create main page

  Personal Blog Static Site Generator

  Testing
    Add new fonts
    Have external image site ready
-}


