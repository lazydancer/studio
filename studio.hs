import Text.Pandoc
import Control.Monad (forM_, join, mapM)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, setCurrentDirectory, copyFile)
import System.FilePath (takeExtension, takeFileName, dropExtension)
import System.IO (writeFile)
import Data.List (intercalate)
import Data.Char (toLower)
import Control.Applicative ((<$>))
import GHC.Exts (sortWith)

main :: IO () 
main = do
  --Init
  setCurrentDirectory "/Users/james/Dropbox/Projects/Site/Studio"
  createDirectoryIfMissing False "Output"
  template <- readFile "template.html"
  
  --Get Articles
  articles <- filter (`notElem` [".","..",".DS_Store"]) <$> getDirectoryContents "Articles"

  --Build Articles and TOC
  readArticles <- mapM readFile $ map ("Articles/" ++) articles 
  let pandocArticles = map (readMarkdown def) readArticles
  unorderedList <- mapM (getItem template) pandocArticles
  let list = orderList unorderedList
  let html = writeHtmlString (siteOptions template) (tocWrap list) 
  writeFile "Output/index.html" html

  --Move over static files  
  files <-  filter ((`elem` [".css",".js",".png",".jpg"]) . takeExtension) <$> getDirectoryContents "."
  forM_ files (\x -> copyFile x ("Output/" ++ x))
  

--Write the article and return information for TOC
getItem :: String -> Pandoc -> IO ([Inline],[Block])
getItem template pandoc = do
  let title = docTitle $ meta pandoc
  let date = docDate $ meta pandoc  
  let year = inlineStr $ [last date]

  let html = writeHtmlString (siteOptions template) pandoc
  createDirectoryIfMissing True $ "Output/" ++ year ++ "/" ++ urlName title 
  writeFile ("Output/" ++ year ++ "/" ++ urlName title ++ "/index.html") html

  return (date,[Plain 
            ([RawInline "html" "<span>"] ++ 
              date ++
                [RawInline "html" "</span>"] ++ 
                  [Link title 
                    ("/" ++ year ++ "/" ++ urlName title,"")])])


--Converting and then sorting with the first element
--The second element is returned
orderList :: [([Inline],[Block])] ->  [[Block]]
orderList = reverse . snd . unzip . sortWith fst . map ea 
  where ea (fs,ls) = (dateOrd $ inlineStrb fs, ls)

--Convert docDate into a number to be able to order
--Format: Jan 02 2013
dateOrd :: [String] -> Integer
dateOrd [month,day,year] = read $ join [year,monthCnvt month,day] 

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


--Convert Pandocs Inline to a string with " " for Space, or into words, or
-- into a urlName format
inlineStr :: [Inline] -> String
inlineStr = foldl fn ""
  where fn ys (Str x) = ys ++ x 
        fn ys (Space ) = ys ++ " "

inlineStrb :: [Inline] -> [String]
inlineStrb = words . inlineStr 
  
urlName :: [Inline] -> String
urlName = map toLower . intercalate "-" . inlineStrb 


meta :: Pandoc -> Meta
meta (Pandoc x _) = x

siteOptions :: String -> WriterOptions
siteOptions template = def { writerStandalone = True, writerTemplate = template }

--Wraps the list into pandoc format to be converted to HTML
tocWrap :: [[Block]] -> Pandoc
tocWrap list = Pandoc Meta{docTitle = [], docAuthors = [], docDate = []} 
                ([Plain [RawInline "html" "<div class=\"toc\">"]] ++ 
                  [BulletList list] ++ [Plain [RawInline "html" "</div>"]]) 
