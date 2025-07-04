module HsFind.FileResult
  ( FileResult(..)
  , blankFileResult
  , colorizeString
  , compareFileResultsByLastMod
  , compareFileResultsByName
  , compareFileResultsByPath
  , compareFileResultsBySize
  , compareFileResultsByType
  , fileResultToString
  , formatDirectory
  , formatFileName
  , formatFilePath
  , formatFileResult
  , newFileResult
  , newFileResultWithSize
  , newFileResultWithSizeAndLastMod
  , sortFileResults
  ) where

import Data.Char (toLower)
import Data.List (sortBy)
import Data.Time (UTCTime)
import System.FilePath ((</>), dropFileName, splitPath, takeDirectory, takeFileName)
import Text.Regex.PCRE

import HsFind.Color (green, reset)
import HsFind.FileTypes
import HsFind.FindSettings
import HsFind.FileUtil (getExtensionIndex)

data FileResult = FileResult {
                               fileResultContainers :: [FilePath]
                             , fileResultPath :: FilePath
                             , fileResultType :: FileType
                             , fileResultSize :: Integer
                             , fileLastMod :: Maybe UTCTime
                             } deriving (Show, Eq)

blankFileResult :: FileResult
blankFileResult = FileResult {
                               fileResultContainers=[]
                             , fileResultPath=""
                             , fileResultType=Unknown
                             , fileResultSize=0
                             , fileLastMod=Nothing
                             }

newFileResult :: FilePath -> FileType -> FileResult
newFileResult fp ft = FileResult {
                                   fileResultContainers=[]
                                 , fileResultPath=fp
                                 , fileResultType=ft
                                 , fileResultSize=0
                                 , fileLastMod=Nothing
                                 }

newFileResultWithSize :: FilePath -> FileType -> Integer -> FileResult
newFileResultWithSize fp ft size = FileResult {
                                   fileResultContainers=[]
                                 , fileResultPath=fp
                                 , fileResultType=ft
                                 , fileResultSize=size
                                 , fileLastMod=Nothing
                                 }

newFileResultWithSizeAndLastMod :: FilePath -> FileType -> Integer -> Maybe UTCTime -> FileResult
newFileResultWithSizeAndLastMod fp ft size lastmod = FileResult {
                                                     fileResultContainers=[]
                                                   , fileResultPath=fp
                                                   , fileResultType=ft
                                                   , fileResultSize=size
                                                   , fileLastMod=lastmod
                                                   }

fileResultToString :: FileResult -> String
fileResultToString = fileResultPath

colorizeString :: String -> Int -> Int -> String
colorizeString s startIdx len = 
  prefixS ++ green ++ matchS ++ reset ++ suffixS
  where prefixS =
          if startIdx > 0
          then take startIdx s
          else ""
        suffixS =
          if endIdx < length s
          then drop endIdx s
          else ""
        matchS = take (endIdx - startIdx) $ drop startIdx s
        endIdx = startIdx + len

colorizeDirectory :: FindSettings -> FilePath -> String
colorizeDirectory settings dir = 
  case filter (\p -> dir =~ p :: Bool) (inDirPatterns settings) of
    [] -> dir
    (p:_) -> case getAllMatches (dir =~ p) :: [(Int, Int)] of
      ((mStart, mLen):_) -> colorizeString dir mStart mLen
      [] -> dir

formatDirectory :: FindSettings -> FilePath -> String
formatDirectory settings dir = if colorize settings && hasColorizableSettings
                               then colorizeDirectory settings dir
                               else dir
  where hasColorizableSettings = not $ null (inDirPatterns settings)

colorizeFileName :: FindSettings -> String -> String
colorizeFileName settings fileName = formattedFileName
  where withFilePattern = 
          case filter (\p -> fileName =~ p :: Bool) (inFilePatterns settings) of
            [] -> fileName
            (p:_) -> case getAllMatches (fileName =~ p) :: [(Int, Int)] of
              ((mStart, mLen):_) -> colorizeString fileName mStart mLen
              [] -> fileName
        maxDotIndex = getExtensionIndex withFilePattern
        formattedFileName = if not (null (inExtensions settings)) && maxDotIndex > 0 && maxDotIndex < length withFilePattern -1
                            then colorizeString withFilePattern (maxDotIndex + 1) $ length withFilePattern
                            else withFilePattern

formatFileName :: FindSettings -> String -> String
formatFileName settings fileName = if colorize settings && hasColorizableSettings
                                   then colorizeFileName settings fileName
                                   else fileName
  where hasColorizableSettings = not (null (inFilePatterns settings)) || not (null (inExtensions settings))

formatFilePath :: FindSettings -> FilePath -> String
formatFilePath settings fp = formattedParent </> formattedFile
  where parent = takeDirectory fp
        formattedParent = formatDirectory settings parent
        fileName = takeFileName fp
        formattedFile = formatFileName settings fileName

formatFileResult :: FindSettings -> FileResult -> String
formatFileResult settings result = formatFilePath settings (fileResultPath result)

compareStrings :: FindSettings -> String -> String -> Ordering
compareStrings settings s1 s2 =
  if sortCaseInsensitive settings
  then compare (lower s1) (lower s2)
  else compare s1 s2
  where lower = map toLower

comparePaths :: FindSettings -> String -> String -> Ordering
comparePaths settings p1 p2 =
  if sortCaseInsensitive settings
  then compare (map lower elems1) (map lower elems2)
  else compare elems1 elems2
  where lower = map toLower
        elems1 = splitPath p1
        elems2 = splitPath p2

compareFileResultsByPath :: FindSettings -> FileResult -> FileResult -> Ordering
compareFileResultsByPath settings fr1 fr2 =
  if pcmp == EQ
  then compareStrings settings f1 f2
  else pcmp
  where p1 = dropFileName (fileResultPath fr1)
        p2 = dropFileName (fileResultPath fr2)
        f1 = takeFileName (fileResultPath fr1)
        f2 = takeFileName (fileResultPath fr2)
        pcmp = comparePaths settings p1 p2

compareFileResultsByName :: FindSettings -> FileResult -> FileResult -> Ordering
compareFileResultsByName settings fr1 fr2 =
  if fcmp == EQ
  then comparePaths settings p1 p2
  else fcmp
  where p1 = dropFileName (fileResultPath fr1)
        p2 = dropFileName (fileResultPath fr2)
        f1 = takeFileName (fileResultPath fr1)
        f2 = takeFileName (fileResultPath fr2)
        fcmp = compareStrings settings f1 f2

compareFileResultsBySize :: FindSettings -> FileResult -> FileResult -> Ordering
compareFileResultsBySize settings fr1 fr2 =
  if s1 == s2
  then compareFileResultsByPath settings fr1 fr2
  else compare s1 s2
  where s1 = fileResultSize fr1
        s2 = fileResultSize fr2

compareFileResultsByType :: FindSettings -> FileResult -> FileResult -> Ordering
compareFileResultsByType settings fr1 fr2 =
  if t1 == t2
  then compareFileResultsByPath settings fr1 fr2
  else compare t1 t2
  where t1 = fileResultType fr1
        t2 = fileResultType fr2

compareFileResultsByLastMod :: FindSettings -> FileResult -> FileResult -> Ordering
compareFileResultsByLastMod settings fr1 fr2 =
  if m1 == m2
  then compareFileResultsByPath settings fr1 fr2
  else compare m1 m2
  where m1 = fileLastMod fr1
        m2 = fileLastMod fr2

getCompareFileResultsFunc :: FindSettings -> FileResult -> FileResult -> Ordering
getCompareFileResultsFunc settings =
  case sortResultsBy settings of
   SortByFileName -> compareFileResultsByName settings
   SortByFileSize -> compareFileResultsBySize settings
   SortByFileType -> compareFileResultsByType settings
   SortByLastMod  -> compareFileResultsByLastMod settings
   _              -> compareFileResultsByPath settings

doSortByFileResults :: FindSettings -> [FileResult] -> [FileResult]
doSortByFileResults settings = sortBy $ getCompareFileResultsFunc settings

sortFileResults :: FindSettings -> [FileResult] -> [FileResult]
sortFileResults settings fileResults =
  if sortDescending settings
  then reverse $ doSortByFileResults settings fileResults
  else doSortByFileResults settings fileResults
