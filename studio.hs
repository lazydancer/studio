import Text.Pandoc
import Control.Monad (forM_, join, mapM, mapM_)
import Control.Applicative ((<$>))
import System.Directory (getDirectoryContents, createDirectoryIfMissing, 
                         setCurrentDirectory, copyFile)
import System.FilePath (takeExtension, takeFileName, dropExtension)
import System.IO (writeFile)
import Data.List (intercalate)
import Data.Char (toLower)
import GHC.Exts (sortWith, groupWith)

data Month 
  = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Ord, Enum, Read, Show)

data Page = Page { title :: FilePath    -- Path to words
                 , static  :: [FilePath]  -- List of static files 
                 , date    :: [String]    -- Month,day,year
                 } deriving (Show) 


main :: IO () 
main = do
  --Init
  --setCurrentDirectory "/Users/james/Dropbox/Projects/Site/Studio"
  createDirectoryIfMissing False "Output"
  template <- readFile "template.html"
  
  --Get Articles
  articles <- getDirectoryContents "Articles"
  let articles' = filter (`notElem` [".","..",".DS_Store"]) articles 

  pages <- getPages articles'
  
  --Build Articles
  mapM_ (writePage template) pages

  --Move over the images
    --articles
  mapM_ moveStatic pages
    --base dir
  files <-  filter ((`elem` [".css",".js"]) . takeExtension) <$> getDirectoryContents "."
  forM_ files (\x -> copyFile x ("Output/" ++ x))
  
  --Table of contents, main page
  writeTOC template pages

getPages :: [FilePath] -> IO [Page]
getPages = mapM (\x -> do static <- getStatic ("Articles/" ++ x)
                          date <- getPageDate ("Articles/" ++ x ++ "/words.md")
                          return (Page x static date))

writePage :: String -> Page -> IO ()
writePage template page= do
  let year = last $ date page  

  mdArt <- readFile ("Articles/" ++ title page ++ "/words.md")
  let pandocArt = readMarkdown def mdArt
  let html = writeHtmlString (siteOptions template) pandocArt
  createDirectoryIfMissing True $ "Output/" ++ year ++ "/" ++ title page 
  writeFile ("Output/" ++ year ++ "/" ++ title page ++ "/index.html") html

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

moveStatic :: Page -> IO ()
moveStatic page = mapM_ func (static page)
  where func x = copyFile x ("Output/" ++ (last (date page)) ++ "/" ++ (drop 9 x))
 
writeTOC :: String -> [Page] -> IO ()
writeTOC template pages = do
  list <- mapM getItem pages 
  let list' = orderList list
  let html = writeHtmlString (siteOptions template) (tocWrap list') 
  createDirectoryIfMissing True $ "Output/archives/" 
  writeFile "Output/archives/index.html" html 

--Write the article and return information for TOC
getItem :: Page -> IO ([Inline],[Block])
getItem page = do
  mdArt <- readFile ("Articles/" ++ title page ++ "/words.md")
  let pandoc = readMarkdown def mdArt

  let date = docDate $ meta pandoc  
  let year = unwords $ inlineStr [last date]

  return (date,[Plain 
            ([RawInline "html" "<span>"] ++ 
              date ++
                [RawInline "html" "</span>"] ++ 
                  [Link (docTitle $ meta pandoc)
                    ("/" ++ year ++ "/" ++ title page,"")])])


--Converting and then sorting with the first element
--This is done by converting the date into a number
--The second element is returned
orderList :: [([Inline],[Block])] -> [[Block]]
orderList = reverse . snd . unzip . sortWith fst . map ea 
  where ea (fs,ls) = (dateOrd $ inlineStr fs, ls)

--Convert docDate into a number to be able to order
--Format: Jan 02 2013 = 20130102
dateOrd :: [String] -> Integer
dateOrd [month,day,year] = read $ join [year,monthCnvt month,day] 

--Convert the month to its repective date number in string
monthCnvt :: String -> String 
monthCnvt x | length monthNum < 10 = "0" ++ monthNum
            | otherwise            = monthNum
  where monthNum = show $ fromEnum (read x :: Month)

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
