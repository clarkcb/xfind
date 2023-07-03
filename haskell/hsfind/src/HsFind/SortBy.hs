module HsFind.SortBy
  ( getSortByForName
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
    "name" -> SortByFileName
    "size" -> SortByFileSize
    "type" -> SortByFileType
    "lastmod" -> SortByLastMod
    _ -> SortByFilePath
  where lower = map toLower
