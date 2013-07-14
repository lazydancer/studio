import Text.Pandoc

import Control.Monad (forM_, join)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, setCurrentDirectory, copyFile)
import System.FilePath (takeExtension, takeFileName, dropExtension)
import System.IO (writeFile)

import Control.Applicative ((<$>))
import GHC.Exts (sortWith)

main :: IO () 
main = do
  setCurrentDirectory "/Users/james/Dropbox/Projects/Site/Studio"
  createDirectoryIfMissing False "Output"
  mdFiles <- getArticles
  mapM_ writeArticle mdFiles
  writeTOC mdFiles
  moveStatic

getArticles :: IO [FilePath]
getArticles = filter (`notElem` [".","..",".DS_Store"]) <$> getDirectoryContents "Articles"

writeArticle :: FilePath -> IO ()
writeArticle file = do 
  pandoc <- readMarkdown def <$> readFile ("Articles/" ++ file) 
  let year = inlineStr $ [last $ docDate $ meta pandoc]  
  template <- readFile "template.html"
  let html = writeHtmlString (siteOptions template) pandoc
  createDirectoryIfMissing True $ "Output/" ++ year ++ "/" ++ dropExtension (takeFileName file)
  writeFile ("Output/" ++ year ++ "/" ++ dropExtension (takeFileName file) ++ "/index.html") html

writeTOC :: [FilePath] -> IO ()
writeTOC mdFiles = do
  list <- mapM getItem mdFiles
  let olist = orderList $ listOrd list
  template <- readFile "template.html"
  let tocPan = Pandoc Meta{docTitle = [], docAuthors = [], docDate = []} ([Plain [RawInline "html" "<div class=\"toc\">"]] ++ [BulletList olist] ++ [Plain [RawInline "html" "</div>"]]) 
  let html = writeHtmlString (siteOptions template) tocPan 
  writeFile "Output/index.html" html

moveStatic :: IO ()
moveStatic = do
  cpf <-  filter ((`elem` [".css",".js",".png",".jpg"]) . takeExtension) <$> getDirectoryContents "."
  forM_ cpf (\x -> copyFile x ("Output/" ++ x))

--Orders the toc list by the date, reverse chronological
listOrd :: [([Inline],[Block])] -> [(Integer,[Block])]
listOrd = map ea
  where ea (fs,ls) = (dateOrd $ inlineStrb fs, ls)

--Orders the tuples and returns the a ordered list of TOC elements
orderList :: [(Integer,[Block])] ->  [[Block]]
orderList xs = reverse $ snd $ unzip $ sortWith fst xs 

--Convert docDate into a number to be able to order
--Format: Jan 02 2013
dateOrd :: [String] -> Integer
dateOrd [month,day,year] = read $ join $ map monthCnvt[year,month,day] 

--Convert the month to its repective date number
monthCnvt :: String -> String 
monthCnvt x
  | "Jan" == x = "01"
  | "Feb" == x = "02"
  | "Mar" == x = "03"
  | "Apr" == x = "04"
  | "May" == x = "05"
  | "Jun" == x = "06"
  | "Jul" == x = "07"
  | "Aug" == x = "08"
  | "Sep" == x = "09"
  | "Oct" == x = "10"
  | "Nov" == x = "11"
  | "Dec" == x = "12"
  | otherwise  = x


inlineStr :: [Inline] -> String
inlineStr = foldl fn ""
  where fn ys (Str x) = ys ++ x 
        fn ys (Space ) = ys ++ " "

inlineStrb = words . inlineStr 
  
--The block is returned with an inline date for ordering
getItem :: FilePath -> IO ([Inline],[Block])
getItem file = do
  pandoc <- readMarkdown def <$> readFile ("Articles/" ++ file)
  let title = docTitle $ meta pandoc
  let date = docDate $ meta pandoc  
  let year = inlineStr $ [last date]
  return (date,[Plain 
            ([RawInline "html" "<span>"] ++ 
              date ++
                [RawInline "html" "</span>"] ++ 
                  [Link title 
                    ("/" ++ year ++ "/" ++ dropExtension (takeFileName file),"")])])


meta :: Pandoc -> Meta
meta (Pandoc x _) = x

siteOptions :: String -> WriterOptions
siteOptions template = def { writerStandalone = True, writerTemplate = template }

{-
Pandoc (Meta {docTitle = [Str "James",Space,Str "Pucula"], docAuthors = [], docDate = []}) [Para [Link [] ("/","")],BulletList [[Plain [Str "March",Space,Str "29,",Space,Str "2013",Link [Str "Bitcoin"] ("/2012/bitcoin","")]],[Plain [Str "April",Space,Str "10,",Space,Str "2013",Link [Str "A",Space,Str "longer",Space,Str "Title"] ("/2012/hello-world","")]],[Plain [Str "June",Space,Str "1,",Space,Str "2013",Link [Str "Shrt",Space,Str "Title"] ("/2012/hello-world","")]],[Plain [Str "January",Space,Str "2,",Space,Str "2013",Link [Str "Never",Space,Str "Runs",Space,Str "out",Space,Str "of",Space,Str "gas"] ("/2012/hello-world","")]]]]
-}
