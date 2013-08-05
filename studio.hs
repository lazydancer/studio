import Text.Pandoc
import Control.Monad (forM_, join, mapM)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, setCurrentDirectory, copyFile)
import System.FilePath (takeExtension, takeFileName, dropExtension)
import System.IO (writeFile)
import Data.List (intercalate)
import Data.Char (toLower)
import Control.Applicative ((<$>))
import GHC.Exts (sortWith, groupWith)

data Month 
  = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Ord, Enum, Read, Show)

data Page = Page { article :: FilePath
                 , static  :: [FilePath]
                 , date    :: [String] --Month,day,year
                 } deriving (Show) 

main :: IO () 
main = do
  --Init
  setCurrentDirectory "/Users/james/Dropbox/Projects/Site/Studio"
  createDirectoryIfMissing False "Output"
  template <- readFile "template.html"
  
  --Get Articles
  articles <- getDirectoryContents "Articles"
  let articles' = filter (`notElem` [".","..",".DS_Store"]) articles 
  pages <- getPages articles'
  
  --Build Articles and TOC
  mapM (writePage template) pages

  writeTOC template pages

  --Move over static files  
  files <-  filter ((`elem` [".css",".js"]) . takeExtension) <$> getDirectoryContents "."
  forM_ files (\x -> copyFile x ("Output/" ++ x))
  

writePage :: String -> Page -> IO ()
writePage template page= do
  let year = last $ date page  
  let title =  article page

  mdArt <- readFile ("Articles/" ++ (article page) ++ "/words.md")
  let pandocArt = readMarkdown def mdArt
  let html = writeHtmlString (siteOptions template) pandocArt
  createDirectoryIfMissing True $ "Output/" ++ year ++ "/" ++ title 
  writeFile ("Output/" ++ year ++ "/" ++ title ++ "/index.html") html

writeTOC :: String -> [Page] -> IO ()
writeTOC template pages = do
  list <- mapM getItem pages 
  let list' = orderList list
  let html = writeHtmlString (siteOptions template) (tocWrap list') 
  writeFile "Output/index.html" html

getPages :: [FilePath] -> IO [Page]
getPages = mapM (\x -> do static <- getStatic ("Articles/" ++ x)
                          date <- getPageDate ("Articles/" ++ x ++ "/words.md")
                          return (Page x static date))
    

getPageDate :: FilePath -> IO [String] 
getPageDate article = do
  article' <- readFile article 
  let article'' = readMarkdown def article'
  return $ inlineStr $ docDate $ meta article'' 

getStatic :: FilePath -> IO [FilePath]
getStatic article = do
  list <- getDirectoryContents article
  let list' = filter ((`elem` [".css",".js",".png",".jpg"]) . takeExtension) list
  return $ map ((article ++ "/") ++) list'
  

--Write the article and return information for TOC
getItem :: Page -> IO ([Inline],[Block])
getItem page = do
  mdArt <- readFile ("Articles/" ++ (article page) ++ "/words.md")
  let pandoc = readMarkdown def mdArt

  let title = docTitle $ meta pandoc
  let date = docDate $ meta pandoc  
  let year = unwords $ inlineStr $ [last date]

  return (date,[Plain 
            ([RawInline "html" "<span>"] ++ 
              date ++
                [RawInline "html" "</span>"] ++ 
                  [Link title 
                    ("/" ++ year ++ "/" ++ (article page),"")])])


--Converting and then sorting with the first element
--This is done by converting the date into a number
--The second element is returned
orderList :: [([Inline],[Block])] -> [[Block]]
orderList = reverse . snd . unzip . sortWith fst . map ea 
  where ea (fs,ls) = (dateOrd $ inlineStr fs, ls)

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

inlineStr :: [Inline] -> [String]
inlineStr = foldl fn [] 
  where fn ys (Str x) = ys ++ [x] 
        fn ys (Space ) = ys
  
meta :: Pandoc -> Meta
meta (Pandoc x _) = x

siteOptions :: String -> WriterOptions
siteOptions template = def { writerStandalone = True, writerTemplate = template }

--Wraps the list into pandoc format to be converted to HTML
tocWrap :: [[Block]] -> Pandoc
tocWrap list = Pandoc Meta{docTitle = [], docAuthors = [], docDate = []} 
                ([Plain [RawInline "html" "<div class=\"toc\">"]] ++ 
                  [BulletList list] ++ [Plain [RawInline "html" "</div>"]]) 
