module HsFind.FindSettings
  ( FindSettings(..)
  , defaultFindSettings
  , getSortByForName
  , newExtensions
  , SortBy(..)
  ) where

import Data.List.Split (splitOn)
import Data.Time (UTCTime)

import HsFind.FileTypes (FileType)
import HsFind.FileUtil (normalizeExtension)
import HsFind.SortBy (SortBy(..), getSortByForName)

data FindSettings = FindSettings {
                                   archivesOnly :: Bool
                                 , debug :: Bool
                                 , inArchiveExtensions :: [String]
                                 , inArchiveFilePatterns :: [String]
                                 , inDirPatterns :: [String]
                                 , inExtensions :: [String]
                                 , inFilePatterns :: [String]
                                 , inFileTypes :: [FileType]
                                 , includeArchives :: Bool
                                 , includeHidden :: Bool
                                 , listDirs :: Bool
                                 , listFiles :: Bool
                                 , maxDepth :: Integer
                                 , maxLastMod :: Maybe UTCTime
                                 , maxSize :: Integer
                                 , minDepth :: Integer
                                 , minLastMod :: Maybe UTCTime
                                 , minSize :: Integer
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
                                   , inArchiveExtensions=[]
                                   , inArchiveFilePatterns=[]
                                   , inDirPatterns=[]
                                   , inExtensions=[]
                                   , inFilePatterns=[]
                                   , inFileTypes=[]
                                   , includeArchives=False
                                   , includeHidden=False
                                   , listDirs=False
                                   , listFiles=False
                                   , maxDepth = -1
                                   , maxLastMod=Nothing
                                   , maxSize=0
                                   , minDepth = -1
                                   , minLastMod=Nothing
                                   , minSize=0
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
