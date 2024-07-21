module HsFind.FileResult
  ( FileResult(..)
  , blankFileResult
  , fileResultToString
  , newFileResult
  , newFileResultWithSize
  , newFileResultWithSizeAndLastMod
  ) where

import Data.Time (UTCTime)

import HsFind.FileTypes

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
-- fileResultToString = fileResultToDetailedString

fileResultToDetailedString :: FileResult -> String
fileResultToDetailedString fr = 
  case fileResultSize fr of
    0 -> fileResultPath fr ++ " (type: " ++ getFileTypeName (fileResultType fr) ++ ")"
    size -> fileResultPath fr ++ " (size: " ++ show size ++ ", type: " ++ getFileTypeName (fileResultType fr) ++ ")"
