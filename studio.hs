import Text.Pandoc

-- Getting the contents
import Control.Monad(forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension, replaceExtension)
import System.IO(writeFile)
-- Access to the command line commands, Want to try and do it the haskell way, 
-- a lot easier this way for now
import System.Cmd
import System.Exit

main :: IO ()
main = do
  copyDir
  mdFiles <- getMarkdownPreset
  mapM_ convertMdtoHtml mdFiles


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
--"Out" must exist with a template file
--Quick way to convert, without worrying about the TOC
convertMdtoHtml :: FilePath -> IO () 
convertMdtoHtml file = do
  contents <- readFile file 
  let pandoc = readMarkdown def contents
  template <- readFile "/Users/james/Dropbox/Projects/Site/Studio/In/template.html" 
  let html = writeHtmlString (siteOptions template) pandoc
  writeFile (replaceExtension file ".html") html

--This will take the pandocs for all the articles and make a list that will be
--put into a bulleted list
--TODO: Find out what I need to list to be formated like
--[Plain [Str "March",Space,Str "29,",Space,Str "2013",Link [Str "Bitcoin"] ("/2012/bitcoin","")]]
--Should be able to get the date from the Pandoc instead of the filePath,
--two different ways to find the same thing I will have to fix
a :: [Pandoc] -> [[Block]]
a = undefined

--Pandoc has a BulletList [[Block]]
toc :: Pandoc
toc = Pandoc (Meta [Str "James Pucula"][][]) [BulletList [[Plain [Str "Hello"]],[Plain [Str "Another One"]]]]

--Site Options with all default except following
siteOptions :: String -> WriterOptions
siteOptions template = def { writerStandalone = True, writerTemplate = template }
