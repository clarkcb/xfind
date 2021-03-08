module HsFind.FindFile
  ( FindFile(..)
  , blankFindFile
  , isArchiveFile
  ) where

import HsFind.FileTypes

-- TODO: use this type with all file-based functions
data FindFile = FindFile {
                           findFileContainers :: [FilePath]
                         , findFilePath :: FilePath
                         , findFileType :: FileType
                         } deriving (Show, Eq)

blankFindFile :: FindFile
blankFindFile = FindFile {
                           findFileContainers=[]
                         , findFilePath=""
                         , findFileType=Unknown
                         }

isArchiveFile :: FindFile -> Bool
isArchiveFile ff = findFileType ff == Archive
