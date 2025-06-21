module HsFind.FileResult
  ( FileResult(..)
  , blankFileResult
  , colorizeString
  , fileResultToString
  , formatDirectory
  , formatFileName
  , formatFilePath
  , formatFileResult
  , newFileResult
  , newFileResultWithSize
  , newFileResultWithSizeAndLastMod
  ) where

import Data.Time (UTCTime)
import System.FilePath ((</>), takeDirectory, takeFileName)
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
