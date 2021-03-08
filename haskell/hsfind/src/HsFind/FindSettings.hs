module HsFind.FindSettings
  ( FindSettings(..)
  , defaultFindSettings
  , newExtensions
  ) where

import Data.List.Split (splitOn)

import HsFind.FileTypes
import HsFind.FileUtil (normalizeExtension)

data FindSettings = FindSettings {
                                       archivesOnly :: Bool
                                     , colorize :: Bool
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
                                     , printUsage :: Bool
                                     , printVersion :: Bool
                                     , recursive :: Bool
                                     , paths :: [String]
                                     , verbose :: Bool
                                     } deriving (Show, Eq)

defaultFindSettings :: FindSettings
defaultFindSettings = FindSettings {
                                         archivesOnly=False
                                       , colorize=True
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
                                       , printUsage=False
                                       , printVersion=False
                                       , recursive=True
                                       , paths=[]
                                       , verbose=False
                                       }


newExtensions :: String -> [String]
newExtensions x | ',' `elem` x = map normalizeExtension $ removeBlank (splitOn "," x)
                | otherwise    = [normalizeExtension x]
  where removeBlank :: [String] -> [String]
        removeBlank = filter (/="")
