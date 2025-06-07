module HsFind.FileResult
  ( FileResult(..)
  , blankFileResult
  , fileResultToString
  , formatDirectory
  , formatFileName
  , formatFilePath
  , formatFileResult
  , newFileResult
  , newFileResultWithSize
  , newFileResultWithSizeAndLastMod
  ) where

import Data.List (elemIndices, isPrefixOf)
import Data.Time (UTCTime)
import System.FilePath ((</>), dropFileName, splitPath, takeBaseName, takeDirectory, takeFileName)
import Text.Printf (FormatParse(fpChar))
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
  prefix ++ green ++ match ++ reset ++ suffix
  where prefix =
          if startIdx > 0
          then take startIdx s
          else ""
        suffix =
          if endIdx < length s
          then drop endIdx s
          else ""
        match = take (endIdx - startIdx) $ drop startIdx s
        endIdx = startIdx + len

formatDirectory :: FindSettings -> FilePath -> String
formatDirectory settings dir = if not (colorize settings) || null (inDirPatterns settings)
                               then dir
                               else formattedDirectory
  where formattedDirectory = 
          case filter (\p -> dir =~ p :: Bool) (inDirPatterns settings) of
            [] -> dir
            (p:_) -> case getAllMatches (dir =~ p) :: [(Int, Int)] of
              ((mStart, mLen):_) -> colorizeString dir mStart mLen
              [] -> dir

formatFileName :: FindSettings -> String -> String
formatFileName settings fileName = if not (colorize settings) || null (inFilePatterns settings)
                                   then fileName
                                   else formattedFileName
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

formatFilePath :: FindSettings -> FilePath -> String
formatFilePath settings fp = formattedParent </> formattedFile
  where parent = takeDirectory fp
        formattedParent = formatDirectory settings parent
        fileName = takeFileName fp
        formattedFile = formatFileName settings fileName

formatFileResult :: FindSettings -> FileResult -> String
formatFileResult settings result = formatFilePath settings (fileResultPath result)
