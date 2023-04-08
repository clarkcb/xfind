module HsFind.FindSettings
  ( FindSettings(..)
  , defaultFindSettings
  , getSortByForName
  , newExtensions
  , SortBy(..)
  ) where

import Data.Char (toLower)
import Data.List.Split (splitOn)

import HsFind.FileTypes (FileType)
import HsFind.FileUtil (normalizeExtension)

data SortBy = SortByFilePath
            | SortByFileName
            | SortByFileType
  deriving (Show, Eq)

getSortByForName :: String -> SortBy
getSortByForName sortByName =
  case lower sortByName of
    "name" -> SortByFileName
    "type" -> SortByFileType
    _ -> SortByFilePath
  where lower = map toLower

data FindSettings = FindSettings {
                                   archivesOnly :: Bool
                                 , debug :: Bool
                                 , excludeHidden :: Bool
                                 , inArchiveExtensions :: [String]
                                 , inArchiveFilePatterns :: [String]
                                 , inDirPatterns :: [String]
                                 , inExtensions :: [String]
                                 , inFilePatterns :: [String]
                                 , inFileTypes :: [FileType]
                                 , includeArchives :: Bool
                                 , listDirs :: Bool
                                 , listFiles :: Bool
                                 , outArchiveExtensions :: [String]
                                 , outArchiveFilePatterns :: [String]
                                 , outDirPatterns :: [String]
                                 , outExtensions :: [String]
                                 , outFilePatterns :: [String]
                                 , outFileTypes :: [FileType]
                                 , paths :: [String]
                                 , printUsage :: Bool
                                 , printVersion :: Bool
                                 , recursive :: Bool
                                 , sortCaseInsensitive :: Bool
                                 , sortDescending :: Bool
                                 , sortResultsBy :: SortBy
                                 , verbose :: Bool
                                 } deriving (Show, Eq)

defaultFindSettings :: FindSettings
defaultFindSettings = FindSettings {
                                     archivesOnly=False
                                   , debug=False
                                   , excludeHidden=True
                                   , inArchiveExtensions=[]
                                   , inArchiveFilePatterns=[]
                                   , inDirPatterns=[]
                                   , inExtensions=[]
                                   , inFilePatterns=[]
                                   , inFileTypes=[]
                                   , includeArchives=False
                                   , listDirs=False
                                   , listFiles=False
                                   , outArchiveExtensions=[]
                                   , outArchiveFilePatterns=[]
                                   , outDirPatterns=[]
                                   , outExtensions=[]
                                   , outFilePatterns=[]
                                   , outFileTypes=[]
                                   , paths=[]
                                   , printUsage=False
                                   , printVersion=False
                                   , recursive=True
                                   , sortCaseInsensitive=False
                                   , sortDescending=False
                                   , sortResultsBy=SortByFilePath
                                   , verbose=False
                                   }


newExtensions :: String -> [String]
newExtensions x | ',' `elem` x = map normalizeExtension $ removeBlank (splitOn "," x)
                | otherwise    = [normalizeExtension x]
  where removeBlank :: [String] -> [String]
        removeBlank = filter (/="")
