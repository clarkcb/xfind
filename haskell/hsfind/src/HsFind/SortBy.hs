module HsFind.SortBy
  ( getSortByForName
  , sortByToString
  , SortBy(..)
  ) where

import Data.Char (toLower)

data SortBy = SortByFilePath
            | SortByFileName
            | SortByFileSize
            | SortByFileType
            | SortByLastMod
  deriving (Show, Eq)

getSortByForName :: String -> SortBy
getSortByForName sortByName =
  case lower sortByName of
    "filename" -> SortByFileName
    "name" -> SortByFileName
    "filesize" -> SortByFileSize
    "size" -> SortByFileSize
    "filetype" -> SortByFileType
    "type" -> SortByFileType
    "lastmod" -> SortByLastMod
    _ -> SortByFilePath
  where lower = map toLower

sortByToString :: SortBy -> String
sortByToString sb =
  case sb of
    SortByFileName -> "filename"
    SortByFileSize -> "filesize"
    SortByFileType -> "filetype"
    SortByLastMod -> "lastmod"
    _ -> "filepath"
