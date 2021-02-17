module HsFind.FindFile
  ( FindFile(..)
  , isArchiveFile
  , isFindableFile
  ) where

import HsFind.FileTypes

-- TODO: use this type with all file-based functions
data FindFile = FindFile {
                                findFileContainers :: [FilePath]
                              , findFilePath :: FilePath
                              , findFileType :: FileType
                              } deriving (Show, Eq)

isArchiveFile :: FindFile -> Bool
isArchiveFile sf = findFileType sf == Archive

isFindableFile :: FindFile -> Bool
isFindableFile sf = isFindableFileType (findFileType sf)
