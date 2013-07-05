import Text.Pandoc

import Control.Monad (forM_)
import System.Directory (doesDirectoryExist, getDirectoryContents,createDirectoryIfMissing, setCurrentDirectory, copyFile)
import System.FilePath ((</>), takeExtension, replaceExtension, takeFileName, dropExtension)
import System.IO(writeFile)

data Month =
    January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | November
  | December
  deriving (Eq, Show, Read, Ord)


main :: IO () 
main = do
  --init
  setCurrentDirectory "/Users/james/Dropbox/Projects/Site/Studio"
  createDirectoryIfMissing False "Output"

  --Convert the articles
  mdFiles <- getArticles

  mapM_ writeArticle mdFiles

  writeTOC mdFiles

  --Move supporting files over
  names <- getDirectoryContents "."
  let cpf = filter (flip elem [".css",".js",".png",".jpg"] . takeExtension) names
  forM_ cpf (\x -> copyFile x ("Output/" ++ x))

getArticles :: IO [FilePath]
getArticles = do
  names <- getDirectoryContents "Articles"
  return $ filter (`notElem` [".","..",".DS_Store"]) names

--Converts and creates the articles
writeArticle :: FilePath -> IO ()
writeArticle file = do 

  let fname = takeFileName file
  contents <- readFile ("Articles/" ++ file)

  let pandoc = readMarkdown def contents
  
  let year = str $ last $ docDate $ meta pandoc  

  template <- readFile "template.html"
  let html = writeHtmlString (siteOptions template) pandoc
  
  createDirectoryIfMissing True ("Output/" ++ year ++ "/" ++ dropExtension fname)
  writeFile ("Output/" ++ year ++ "/" ++ dropExtension fname ++ "/index.html") html

writeTOC :: [FilePath] -> IO ()
writeTOC mdFiles = do
  list <- mapM getItem mdFiles

--Order the list and remove first element
{-
  template <- readFile "template.html"
  let tocPan = Pandoc Meta{docTitle = [], docAuthors = [], docDate = []} ([Plain [RawInline "html" "<div class=\"toc\">"]] ++ [BulletList list] ++ [Plain [RawInline "html" "</div>"]]) 
  let html = writeHtmlString (siteOptions template) tocPan 
  writeFile "Output/index.html" html
-}
  return ()



cnvtLineToString :: [Inline] -> String
cnvtLineToString xs = foldl fn "" xs
  where fn ys (Str x) = ys ++ x 
        fn ys (Space ) = ys ++ " "
--foldl :: (a -> b -> a) -> a -> [b] -> a
  
getItem :: FilePath -> IO ([Inline],[Block])
getItem file = do
  let fname = takeFileName file
  
  contents <- readFile ("Articles/" ++ file)
  let pandoc = readMarkdown def contents
  
  let name = cnvtLineToString $ docTitle $ meta pandoc
  let date = docDate $ meta pandoc  
  let year = str $ last date
  
  return (date,[Plain 
            ([RawInline "html" "<span>"] ++ 
              date ++
                [RawInline "html" "</span>"] ++ 
                  [Link [Str name] 
                    ("/" ++ year ++ "/" ++ dropExtension fname,"")])])


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
