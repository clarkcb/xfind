{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module HsFind.FileTypes
  ( FileType(..)
  , getFileTypeForName
  , getFileTypeName
  , getDbFileType
  , getDbFileTypes
  , isArchiveFilePath
  , isAudioFilePath
  , isBinaryFilePath
  , isCodeFilePath
  , isFontFilePath
  , isImageFilePath
  , isTextFilePath
  , isVideoFilePath
  , isXmlFilePath
  , isUnknownFilePath
  ) where

import Data.Char (toLower)
import Data.List (intercalate, nub, partition)
import qualified Data.Text as T
import System.FilePath (takeFileName)

import Database.SQLite.Simple

import HsFind.FileUtil (getExtension)
import Data.Maybe (mapMaybe)

data FileType = Unknown
              | Archive
              | Audio
              | Binary
              | Code
              | Font
              | Image
              | Text
              | Video
              | Xml
  deriving (Show, Eq, Ord)

fileTypes :: [FileType]
fileTypes = [Unknown, Archive, Audio, Binary, Code, Font, Image, Text, Video, Xml]

getFileTypeForName :: String -> FileType
getFileTypeForName typeName =
  case lower typeName of
    "archive" -> Archive
    "audio" -> Audio
    "binary" -> Binary
    "code" -> Code
    "font" -> Font
    "image" -> Image
    "text" -> Text
    "video" -> Video
    "xml" -> Xml
    _ -> Unknown
  where lower = map toLower

getFileTypeName :: FileType -> String
getFileTypeName ft =
  case ft of
    Archive -> "archive"
    Audio -> "audio"
    Binary -> "binary"
    Code -> "code"
    Font -> "font"
    Image -> "image"
    Text -> "text"
    Video -> "video"
    Xml -> "xml"
    _ -> "unknown"

batchSize :: Int
batchSize = 500

getDbFileTypesForQueryAndElems :: Connection -> [String] -> [String] -> IO [(String, FileType)]
getDbFileTypesForQueryAndElems conn queryElems [] = return []
getDbFileTypesForQueryAndElems conn queryElems elems = do
  let uniqElems = nub elems
  getFileTypesForElems uniqElems []
  where getFileTypesForElems :: [String] -> [(String, FileType)] -> IO [(String, FileType)]
        getFileTypesForElems elems elemsAndTypes =
          case elems of
            [] -> return elemsAndTypes
            _ -> do
              let elemBatch = take batchSize elems
              let placeholders = intercalate ", " $ replicate (length elemBatch) "?" :: String
              let selectStr = concatMap (\q -> if q == "?" then placeholders else q) queryElems :: String
              let selectText = T.pack selectStr
              let selectQuery = Query {fromQuery=selectText}
              rows <- query conn selectQuery elemBatch :: IO [(String, Int)]
              let dbElemBatchTypes = map (\(e, i) -> (e, fileTypes !! (i - 1))) rows
              let elemBatchAndTypes = getDbFileTypesForElems elemBatch dbElemBatchTypes []
              getFileTypesForElems (drop batchSize elems) $ elemsAndTypes ++ elemBatchAndTypes
        getDbFileTypesForElems :: [String] -> [(String, FileType)] -> [(String, FileType)] -> [(String, FileType)]
        getDbFileTypesForElems elems dbFileElemTypes elemsAndTypes =
          case elems of
            [] -> elemsAndTypes
            (e:elems') ->
              case lookup e dbFileElemTypes of
                Just ft -> getDbFileTypesForElems elems' dbFileElemTypes $ elemsAndTypes ++ [(e, ft)]
                Nothing -> getDbFileTypesForElems elems' dbFileElemTypes $ elemsAndTypes ++ [(e, Unknown)]

getDbFileTypesForFileNames :: Connection -> [FilePath] -> IO [(String, FileType)]
getDbFileTypesForFileNames conn [] = return []
getDbFileTypesForFileNames conn filePaths = do
  let queryElems = ["SELECT name, file_type_id FROM file_name WHERE name IN (", "?", ")"]
  let fileNames = map takeFileName filePaths
  getDbFileTypesForQueryAndElems conn queryElems fileNames

getDbFileTypeForFileName :: Connection -> FilePath -> IO FileType
getDbFileTypeForFileName conn fileName = do
  row <- query conn "SELECT file_type_id FROM file_name WHERE name = ?" (Only (fileName :: String)) :: IO [Only Int]
  case row of
    [Only i] -> return $ fileTypes !! (i - 1)
    _ -> return Unknown

getDbFileTypesForExtensions :: Connection -> [String] -> IO [(String, FileType)]
getDbFileTypesForExtensions conn [] = return []
getDbFileTypesForExtensions conn fileExts = do
  let queryElems = ["SELECT extension, file_type_id FROM file_extension WHERE extension IN (", "?", ")"]
  getDbFileTypesForQueryAndElems conn queryElems fileExts

getDbFileTypeForExtension :: Connection -> String -> IO FileType
getDbFileTypeForExtension conn fileExt = do
  row <- query conn "SELECT file_type_id FROM file_extension WHERE extension = ?" (Only (fileExt :: String)) :: IO [Only Int]
  case row of
    [Only i] -> return $ fileTypes !! (i - 1)
    _ -> return Unknown

getDbFileType :: Connection -> FilePath -> IO FileType
getDbFileType conn f = do
  ft <- getDbFileTypeForFileName conn (takeFileName f)
  case ft of
    Unknown -> case getExtension f of
                  Just x -> getDbFileTypeForExtension conn x
                  Nothing -> return Unknown
    _  -> return ft

-- getDbFileTypes :: Connection -> [FilePath] -> IO [FileType]
-- getDbFileTypes conn filePaths = do
--   mapM (getDbFileType conn) filePaths

getDbFileTypes :: Connection -> [FilePath] -> IO [FileType]
getDbFileTypes conn filePaths = do
  dbFileTypesForFileNames <- getDbFileTypesForFileNames conn $ map takeFileName filePaths
  let (knownTypesForFileNames, unknownTypesForFileNames) = partition (\(name, ft) -> ft /= Unknown) dbFileTypesForFileNames
  let unknownFileNames = map fst unknownTypesForFileNames
  dbFileTypesForExts <- getDbFileTypesForExtensions conn $ mapMaybe getExtension unknownFileNames
  return $ getFilePathsWithFileTypes filePaths knownTypesForFileNames dbFileTypesForExts []
  where getFilePathsWithFileTypes :: [FilePath] -> [(FilePath, FileType)] -> [(FilePath, FileType)] -> [(FilePath, FileType)] -> [FileType]
        getFilePathsWithFileTypes paths knownTypesForFileNames dbFileTypesForExts pathsAndTypes =
          case paths of
            [] -> map snd pathsAndTypes
            (path:paths') ->
              case lookup (takeFileName path) knownTypesForFileNames of
                Just ft -> getFilePathsWithFileTypes paths' knownTypesForFileNames dbFileTypesForExts $ pathsAndTypes ++ [(path, ft)]
                Nothing ->
                  case getExtension path of
                    Just ext ->
                      case lookup ext dbFileTypesForExts of
                        Just ft -> getFilePathsWithFileTypes paths' knownTypesForFileNames dbFileTypesForExts $ pathsAndTypes ++ [(path, ft)]
                        Nothing -> getFilePathsWithFileTypes paths' knownTypesForFileNames dbFileTypesForExts $ pathsAndTypes ++ [(path, Unknown)]
                    Nothing -> getFilePathsWithFileTypes paths' knownTypesForFileNames dbFileTypesForExts $ pathsAndTypes ++ [(path, Unknown)]

hasFilePathFileType :: Connection -> FilePath -> FileType -> IO Bool
hasFilePathFileType conn fp ft = do
  dbFileType <- getDbFileType conn fp
  return $ dbFileType == ft

isUnknownFilePath :: Connection -> FilePath -> IO Bool
isUnknownFilePath conn fp = do
  hasFilePathFileType conn fp Unknown

isArchiveFilePath :: Connection -> FilePath -> IO Bool
isArchiveFilePath conn fp = do
  hasFilePathFileType conn fp Archive

isAudioFilePath :: Connection -> FilePath -> IO Bool
isAudioFilePath conn fp = do
  hasFilePathFileType conn fp Audio

isBinaryFilePath :: Connection -> FilePath -> IO Bool
isBinaryFilePath conn fp = do
  hasFilePathFileType conn fp Binary

isCodeFilePath :: Connection -> FilePath -> IO Bool
isCodeFilePath conn fp = do
  hasFilePathFileType conn fp Code

isFontFilePath :: Connection -> FilePath -> IO Bool
isFontFilePath conn fp = do
  hasFilePathFileType conn fp Font

isImageFilePath :: Connection -> FilePath -> IO Bool
isImageFilePath conn fp = do
  hasFilePathFileType conn fp Image

isTextFilePath :: Connection -> FilePath -> IO Bool
isTextFilePath conn fp = do
  dbFileType <- getDbFileType conn fp
  return $ dbFileType `elem` [Text, Code, Xml]

isVideoFilePath :: Connection -> FilePath -> IO Bool
isVideoFilePath conn fp = do
  hasFilePathFileType conn fp Video

isXmlFilePath :: Connection -> FilePath -> IO Bool
isXmlFilePath conn fp = do
  hasFilePathFileType conn fp Xml
