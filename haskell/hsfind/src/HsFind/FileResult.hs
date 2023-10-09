module HsFind.FileResult
  ( FileResult(..)
  , blankFileResult
  , isArchiveFile
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
                             , fileResultMimeType :: String
                             , fileResultSize :: Integer
                             , fileLastMod :: Maybe UTCTime
                             } deriving (Show, Eq)

blankFileResult :: FileResult
blankFileResult = FileResult {
                               fileResultContainers=[]
                             , fileResultPath=""
                             , fileResultType=Unknown
                             , fileResultMimeType=""
                             , fileResultSize=0
                             , fileLastMod=Nothing
                             }

newFileResult :: FilePath -> FileType -> FileResult
newFileResult fp ft = FileResult {
                                   fileResultContainers=[]
                                 , fileResultPath=fp
                                 , fileResultType=ft
                                 , fileResultMimeType=""
                                 , fileResultSize=0
                                 , fileLastMod=Nothing
                                 }

newFileResultWithSize :: FilePath -> FileType -> Integer -> FileResult
newFileResultWithSize fp ft size = FileResult {
                                   fileResultContainers=[]
                                 , fileResultPath=fp
                                 , fileResultType=ft
                                 , fileResultMimeType=""
                                 , fileResultSize=size
                                 , fileLastMod=Nothing
                                 }

newFileResultWithSizeAndLastMod :: FilePath -> FileType -> Integer -> Maybe UTCTime -> FileResult
newFileResultWithSizeAndLastMod fp ft size lastmod = FileResult {
                                                     fileResultContainers=[]
                                                   , fileResultPath=fp
                                                   , fileResultType=ft
                                                   , fileResultMimeType=""
                                                   , fileResultSize=size
                                                   , fileLastMod=lastmod
                                                   }

isArchiveFile :: FileResult -> Bool
isArchiveFile fr = fileResultType fr == Archive
