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
  setCurrentDirectory "/Users/james/Dropbox/Projects/Site/Studio"
  createDirectoryIfMissing False "Output"
  template <- readFile "template.html"
  
  articles <- filter (`notElem` [".","..",".DS_Store"]) <$> getDirectoryContents "Articles"

  unparsedArt <- mapM readFile $ map ("Articles/" ++) articles 
  let pandocArt = map (readMarkdown def) unparsedArt
  list <- mapM (getItem template) pandocArt 
  let tocPan = Pandoc Meta{docTitle = [], docAuthors = [], docDate = []} ([Plain [RawInline "html" "<div class=\"toc\">"]] ++ [BulletList (orderList list)] ++ [Plain [RawInline "html" "</div>"]]) 
  let html = writeHtmlString (siteOptions template) tocPan 
  writeFile "Output/index.html" html
  
  cpf <-  filter ((`elem` [".css",".js",".png",".jpg"]) . takeExtension) <$> getDirectoryContents "."
  forM_ cpf (\x -> copyFile x ("Output/" ++ x))

  
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

--Orders the tuples and returns the a ordered list of TOC elements
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

{-
Pandoc (Meta {docTitle = [Str "James",Space,Str "Pucula"], docAuthors = [], docDate = []}) [Para [Link [] ("/","")],BulletList [[Plain [Str "March",Space,Str "29,",Space,Str "2013",Link [Str "Bitcoin"] ("/2012/bitcoin","")]],[Plain [Str "April",Space,Str "10,",Space,Str "2013",Link [Str "A",Space,Str "longer",Space,Str "Title"] ("/2012/hello-world","")]],[Plain [Str "June",Space,Str "1,",Space,Str "2013",Link [Str "Shrt",Space,Str "Title"] ("/2012/hello-world","")]],[Plain [Str "January",Space,Str "2,",Space,Str "2013",Link [Str "Never",Space,Str "Runs",Space,Str "out",Space,Str "of",Space,Str "gas"] ("/2012/hello-world","")]]]]
-}
