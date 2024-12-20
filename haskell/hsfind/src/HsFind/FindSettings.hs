module HsFind.FindSettings
  ( FindSettings(..)
  , defaultFindSettings
  , findSettingsToString
  , getSortByForName
  , needFileSizes
  , needLastMods
  , newExtensions
  , SortBy(..)
  ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Time (UTCTime)

import HsFind.FileTypes (FileType, getFileTypeName)
import HsFind.FileUtil (normalizeExtension)
import HsFind.SortBy (SortBy(..), getSortByForName, sortByToString)

data FindSettings = FindSettings {
                                   archivesOnly :: Bool
                                 , debug :: Bool
                                 , followSymlinks :: Bool
                                 , inArchiveExtensions :: [String]
                                 , inArchiveFilePatterns :: [String]
                                 , inDirPatterns :: [String]
                                 , inExtensions :: [String]
                                 , inFilePatterns :: [String]
                                 , inFileTypes :: [FileType]
                                 , includeArchives :: Bool
                                 , includeHidden :: Bool
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
                                 , printDirs :: Bool
                                 , printFiles :: Bool
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
                                   , followSymlinks=False
                                   , inArchiveExtensions=[]
                                   , inArchiveFilePatterns=[]
                                   , inDirPatterns=[]
                                   , inExtensions=[]
                                   , inFilePatterns=[]
                                   , inFileTypes=[]
                                   , includeArchives=False
                                   , includeHidden=False
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
                                   , printDirs=False
                                   , printFiles=False
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

findSettingsToString :: FindSettings -> String
findSettingsToString settings = 
  "FindSettings(" ++
  "archivesOnly=" ++ show (archivesOnly settings) ++
  ", debug=" ++ show (debug settings) ++
  ", followSymlinks=" ++ show (followSymlinks settings) ++
  ", inArchiveExtensions=" ++ listToString (inArchiveExtensions settings) ++
  ", inArchiveFilePatterns=" ++ listToString (inArchiveFilePatterns settings) ++
  ", inDirPatterns=" ++ listToString (inDirPatterns settings) ++
  ", inExtensions=" ++ listToString (inExtensions settings) ++
  ", inFilePatterns=" ++ listToString (inFilePatterns settings) ++
  ", inFileTypes=" ++ fileTypesToString (inFileTypes settings) ++
  ", includeArchives=" ++ show (includeArchives settings) ++
  ", includeHidden=" ++ show (includeHidden settings) ++
  ", maxDepth=" ++ show (maxDepth settings) ++
  ", maxLastMod=" ++ lastModToString (maxLastMod settings) ++
  ", maxSize=" ++ show (maxSize settings) ++
  ", minDepth=" ++ show (minDepth settings) ++
  ", minLastMod=" ++ lastModToString (minLastMod settings) ++
  ", minSize=" ++ show (minSize settings) ++
  ", outArchiveExtensions=" ++ listToString (outArchiveExtensions settings) ++
  ", outArchiveFilePatterns=" ++ listToString (outArchiveFilePatterns settings) ++
  ", outDirPatterns=" ++ listToString (outDirPatterns settings) ++
  ", outExtensions=" ++ listToString (outExtensions settings) ++
  ", outFilePatterns=" ++ listToString (outFilePatterns settings) ++
  ", outFileTypes=" ++ fileTypesToString (outFileTypes settings) ++
  ", paths=" ++ listToString (paths settings) ++
  ", printDirs=" ++ show (printDirs settings) ++
  ", printFiles=" ++ show (printFiles settings) ++
  ", printUsage=" ++ show (printUsage settings) ++
  ", printVersion=" ++ show (printVersion settings) ++
  ", recursive=" ++ show (recursive settings) ++
  ", sortBy=" ++ sortByToString (sortResultsBy settings) ++
  ", sortCaseInsensitive=" ++ show (sortCaseInsensitive settings) ++
  ", sortDescending=" ++ show (sortDescending settings) ++
  ", verbose=" ++ show (verbose settings) ++
  ")"
  where listToString lst | null lst = "[]"
                         | otherwise = "[\"" ++ intercalate "\", \"" lst ++ "\"]"
        fileTypesToString fts = "[" ++ intercalate ", " (fileTypeNames fts) ++ "]"
        fileTypeNames = Prelude.map getFileTypeName
        lastModToString :: Maybe UTCTime -> String
        lastModToString Nothing = "0"
        lastModToString (Just t) = show t

needFileSizes :: FindSettings -> Bool
needFileSizes settings = minSize settings > 0 || maxSize settings > 0 || sortResultsBy settings == SortByFileSize

needLastMods :: FindSettings -> Bool
needLastMods settings = isJust (minLastMod settings) || isJust (maxLastMod settings) || sortResultsBy settings == SortByLastMod
