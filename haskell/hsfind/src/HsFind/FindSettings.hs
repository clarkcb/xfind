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
                                     , firstMatch :: Bool
                                     , inArchiveExtensions :: [String]
                                     , inArchiveFilePatterns :: [String]
                                     , inDirPatterns :: [String]
                                     , inExtensions :: [String]
                                     , inFilePatterns :: [String]
                                     , inFileTypes :: [FileType]
                                     , inLinesAfterPatterns :: [String]
                                     , inLinesBeforePatterns :: [String]
                                     , linesAfter :: Int
                                     , linesAfterToPatterns :: [String]
                                     , linesAfterUntilPatterns :: [String]
                                     , linesBefore :: Int
                                     , listDirs :: Bool
                                     , listFiles :: Bool
                                     , listLines :: Bool
                                     , maxLineLength :: Int
                                     , multiLineFind :: Bool
                                     , outArchiveExtensions :: [String]
                                     , outArchiveFilePatterns :: [String]
                                     , outDirPatterns :: [String]
                                     , outExtensions :: [String]
                                     , outFilePatterns :: [String]
                                     , outFileTypes :: [FileType]
                                     , outLinesAfterPatterns :: [String]
                                     , outLinesBeforePatterns :: [String]
                                     , printResults :: Bool
                                     , printUsage :: Bool
                                     , printVersion :: Bool
                                     , recursive :: Bool
                                     , findArchives :: Bool
                                     , findPatterns :: [String]
                                     , startPath :: String
                                     , textFileEncoding :: String
                                     , uniqueLines :: Bool
                                     , verbose :: Bool
                                     } deriving (Show, Eq)

defaultFindSettings :: FindSettings
defaultFindSettings = FindSettings {
                                         archivesOnly=False
                                       , colorize=True
                                       , debug=False
                                       , excludeHidden=True
                                       , firstMatch=False
                                       , inArchiveExtensions=[]
                                       , inArchiveFilePatterns=[]
                                       , inDirPatterns=[]
                                       , inExtensions=[]
                                       , inFilePatterns=[]
                                       , inFileTypes=[]
                                       , inLinesAfterPatterns=[]
                                       , inLinesBeforePatterns=[]
                                       , linesAfter=0
                                       , linesAfterToPatterns=[]
                                       , linesAfterUntilPatterns=[]
                                       , linesBefore=0
                                       , listDirs=False
                                       , listFiles=False
                                       , listLines=False
                                       , maxLineLength=200
                                       , multiLineFind=False
                                       , outArchiveExtensions=[]
                                       , outArchiveFilePatterns=[]
                                       , outDirPatterns=[]
                                       , outExtensions=[]
                                       , outFilePatterns=[]
                                       , outFileTypes=[]
                                       , outLinesAfterPatterns=[]
                                       , outLinesBeforePatterns=[]
                                       , printResults=True
                                       , printUsage=False
                                       , printVersion=False
                                       , recursive=True
                                       , findArchives=False
                                       , findPatterns=[]
                                       , startPath=""
                                       , textFileEncoding="utf-8"
                                       , uniqueLines=False
                                       , verbose=False
                                       }


newExtensions :: String -> [String]
newExtensions x | ',' `elem` x = map normalizeExtension $ removeBlank (splitOn "," x)
                | otherwise    = [normalizeExtension x]
  where removeBlank :: [String] -> [String]
        removeBlank = filter (/="")
