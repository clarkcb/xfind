module HsFind.FileResult
  ( FileResult(..)
  , blankFileResult
  , isArchiveFile
  , newFileResult
  ) where

import HsFind.FileTypes

data FileResult = FileResult {
                               fileResultContainers :: [FilePath]
                             , fileResultPath :: FilePath
                             , fileResultType :: FileType
                             } deriving (Show, Eq)

blankFileResult :: FileResult
blankFileResult = FileResult {
                               fileResultContainers=[]
                             , fileResultPath=""
                             , fileResultType=Unknown
                             }

newFileResult :: FilePath -> FileType -> FileResult
newFileResult fp ft = FileResult {
                                   fileResultContainers=[]
                                 , fileResultPath=fp
                                 , fileResultType=ft
                                 }

isArchiveFile :: FileResult -> Bool
isArchiveFile fr = fileResultType fr == Archive
