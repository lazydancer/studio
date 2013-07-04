import Text.Pandoc

import Control.Monad (forM_)
import System.Directory (doesDirectoryExist, getDirectoryContents,createDirectoryIfMissing, setCurrentDirectory, copyFile)
import System.FilePath ((</>), takeExtension, replaceExtension, takeFileName, dropExtension)
import System.IO(writeFile)

--Have list do reverse chronilogical order
--  I think the mapFunc has to much logic in it.
main :: IO () 
main = do
  --init
  setCurrentDirectory "/Users/james/Dropbox/Projects/Site/Studio"
  createDirectoryIfMissing False "Output"

  --Convert the articles
  mdFiles <- getArticles
--let mdFilesOrd = orderingOfmdFiles mdFiles //this is a possiblity
  list <- mapM mapFunc mdFiles

  --Create TOC
  template <- readFile "template.html"
  let tocPan = Pandoc Meta{docTitle = [], docAuthors = [], docDate = []} ([Plain [RawInline "html" "<div class=\"toc\">"]] ++ [BulletList list] ++ [Plain [RawInline "html" "</div>"]]) 
  let html = writeHtmlString (siteOptions template) tocPan 
  writeFile "Output/index.html" html
  
  --Move supporting files over
  names <- getDirectoryContents "."
  let cpf = filter (flip elem [".css",".js",".png",".jpg"] . takeExtension) names
  forM_ cpf (\x -> copyFile x ("Output/" ++ x))

getArticles :: IO [FilePath]
getArticles = do
  names <- getDirectoryContents "Articles"
  return $ filter (`notElem` [".","..",".DS_Store"]) names

--returns information used for the TOC
mapFunc :: FilePath -> IO [Block]
mapFunc file = do 
  let fname = takeFileName file
  
  contents <- readFile ("Articles/" ++ file)
  let pandoc = readMarkdown def contents
  
  let name = str $ head $ docTitle $ meta pandoc
  let date = docDate $ meta pandoc  
  let year = str $ last date
 
  template <- readFile "template.html"
  let html = writeHtmlString (siteOptions template) pandoc
  
  createDirectoryIfMissing True ("Output/" ++ year ++ "/" ++ dropExtension fname)
  writeFile ("Output/" ++ year ++ "/" ++ dropExtension fname ++ "/index.html") html
  
  return [Plain 
            ([RawInline "html" "<span>"] ++ 
              date ++
                [RawInline "html" "</span>"] ++ 
                  [Link [Str name] 
                    ("/" ++ year ++ "/" ++ dropExtension fname,"")])]

meta :: Pandoc -> Meta
meta (Pandoc x _) = x

--only support single words for now
str :: Inline -> String
str (Str x) = x



{-
Pandoc (Meta {docTitle = [Str "James",Space,Str "Pucula"], docAuthors = [], docDate = []}) [Para [Link [] ("/","")],BulletList [[Plain [Str "March",Space,Str "29,",Space,Str "2013",Link [Str "Bitcoin"] ("/2012/bitcoin","")]],[Plain [Str "April",Space,Str "10,",Space,Str "2013",Link [Str "A",Space,Str "longer",Space,Str "Title"] ("/2012/hello-world","")]],[Plain [Str "June",Space,Str "1,",Space,Str "2013",Link [Str "Shrt",Space,Str "Title"] ("/2012/hello-world","")]],[Plain [Str "January",Space,Str "2,",Space,Str "2013",Link [Str "Never",Space,Str "Runs",Space,Str "out",Space,Str "of",Space,Str "gas"] ("/2012/hello-world","")]]]]
-}

--Site Options with all default except following
siteOptions :: String -> WriterOptions
siteOptions template = def { writerStandalone = True, writerTemplate = template }
