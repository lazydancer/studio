import Text.Pandoc

-- Getting the contents
import Control.Monad(forM)
import System.Directory (doesDirectoryExist, getDirectoryContents,createDirectoryIfMissing, setCurrentDirectory)
import System.FilePath ((</>), takeExtension, replaceExtension)
import System.IO(writeFile)
-- Access to the command line commands, Want to try and do it the haskell way, 
-- a lot easier this way for now
import System.Cmd
import System.Exit


main :: IO ()
main = do
  setCurrentDirectory "~/Dropbox/Projects/Site/Studio/"
  createDirectoryIfMissing False "Output"
  mdFiles <- getArticles
  mapM mapFunc mdFiles --mapM :: Monad m => (a -> m b) -> [a] -> m [b] 
  return ()

getArticles :: IO [FilePath]
getArticles = do
  names <- getDirectoryContents "Articles"
  return $ filter (`notElem` [".","..",".DS_Store"]) names

--The block returned is information used for the TOC
mapFunc :: String -> IO Block 
mapFunc = undefined 
  --Get Name
  --Convert to Pandoc
  --Get Pandoc meta docDate
  --Get the year and use it it create a folder if missing
  --Create file
  --Return Block information used in TOC





--Takes a markdown file and writes a html file, same name in same dir
--"Out" must exist with a template file
--Quick way to convert, without worrying about the TOC
convertMdtoHtml :: FilePath -> IO () 
convertMdtoHtml file = do
  contents <- readFile file 
  let pandoc = readMarkdown def contents
  template <- readFile "main.hs" 
  let html = writeHtmlString (siteOptions template) pandoc
  writeFile (replaceExtension file ".html") html

--This will take the pandocs for all the articles and make a list that will be
--put into a bulleted list
--TODO: Find out what I need to list to be formated like
--[Plain [Str "March",Space,Str "29,",Space,Str "2013",Link [Str "Bitcoin"] ("/2012/bitcoin","")]]
--Should be able to get the date from the Pandoc instead of the filePath,
--two different ways to find the same thing I will have to fix

--Title = URL = " " --> "-" lowercase
{-
Pandoc (Meta {docTitle = [Str "James",Space,Str "Pucula"], docAuthors = [], docDate = []}) [Para [Link [] ("/","")],BulletList [[Plain [Str "March",Space,Str "29,",Space,Str "2013",Link [Str "Bitcoin"] ("/2012/bitcoin","")]],[Plain [Str "April",Space,Str "10,",Space,Str "2013",Link [Str "A",Space,Str "longer",Space,Str "Title"] ("/2012/hello-world","")]],[Plain [Str "June",Space,Str "1,",Space,Str "2013",Link [Str "Shrt",Space,Str "Title"] ("/2012/hello-world","")]],[Plain [Str "January",Space,Str "2,",Space,Str "2013",Link [Str "Never",Space,Str "Runs",Space,Str "out",Space,Str "of",Space,Str "gas"] ("/2012/hello-world","")]]]]

createTOC :: [Pandoc] -> [Block]
createTOC articles = undefined

addTOC :: Pandoc -> Block
addTOC pandoc = Plain [docDate, Link docTitle (pathTo,"")] 
  where pathTo = docDateYear ++ "/" ++ docTitle 
--Pandoc has a BulletList [[Block]]
toc :: Pandoc
toc = Pandoc (Meta [Str "James Pucula"][][]) [BulletList [[Plain [Str "Hello"]],[Plain [Str "Another One"]]]]

meta :: Pandoc -> Meta
meta (Pandoc x _) = x

--I will need to write a converter to include Spaces
str :: Str -> String
str (Str x) = x

str $ last $ docDate $ meta pan ==> "2013" 
-}

--Site Options with all default except following
siteOptions :: String -> WriterOptions
siteOptions template = def { writerStandalone = True, writerTemplate = template }
