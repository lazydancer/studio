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


--Copies a directory to a destination from system command, somewhat dangerous (constants added)
--"Out" folder must exist
copyDir :: IO ExitCode
copyDir = system $ "cp -r ~/Dropbox/Projects/Site/Studio/In/* ~/Dropbox/Projects/Site/Studio/Out/"

--;;;;;;Preset Markdown
getMarkdownPreset :: IO [FilePath]
getMarkdownPreset = getMarkdown "/Users/james/Dropbox/Projects/Site/Studio/Out/"

--Finds all the files in a directory and filters out the markdown files
--Need support for .md files
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
convertMdtoHtml :: FilePath -> IO () 
convertMdtoHtml file = do
  contents <- readFile file 
  let pandoc = readMarkdown def contents
  template <- readFile "/Users/james/Dropbox/Projects/Site/Studio/In/template.html" 
  let html = writeHtmlString (siteOptions template) pandoc
  writeFile (replaceExtension file ".html") html

{- Testing -}
convertMdPandoc :: IO () 
convertMdPandoc = do 
  contents <- readFile "/Users/james/Dropbox/Projects/Site/Studio/In/test.html"
  putStrLn ( show (readMarkdown def contents))

--Read the file create a pandoc
--convertMdPandoc :: String -> Pandoc
--convertMdPandoc = readMarkdown def 

--Add a new file for the toc
--Pandoc has a BulletList [[Block]]
toc :: Pandoc
toc = Pandoc (Meta [][][]) [Plain [Str "Hello"]]

createToc :: [Pandoc] -> Pandoc 
createToc = undefined

--Wrap a template around everything 
convertPandocHtml :: Pandoc -> String
convertPandocHtml = undefined

compile :: IO ()
compile = undefined 

--Site Options with all default except following
siteOptions :: String -> WriterOptions
siteOptions template = def { writerStandalone = True, writerTemplate = template }
