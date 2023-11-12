{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction, OverloadedStrings #-}
module HsFind.FileTypes
  ( FileType(..)
  , JsonFileType(..)
  , getFileType
  , getFileTypes
  , getFileTypeForName
  , getFileTypeName
  , getJsonFileTypes
  , fileTypeFromJsonFileTypes
  ) where

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char (toLower)
import Data.Text(pack, unpack, replace)

import GHC.Generics
import Data.Aeson

import System.FilePath (takeFileName)

import HsFind.FileUtil (getExtension, normalizeExtension, getFileString)
import HsFind.Paths_hsfind (getDataFileName)

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

-- searchableFileTypes :: [FileType]
-- searchableFileTypes = [Archive, Binary, Code, Image, Text, Xml]

-- isSearchableFileType :: FileType -> Bool
-- isSearchableFileType t = t `elem` searchableFileTypes

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

data JsonFileType = JsonFileType
    { fileType :: String
    , extensions :: [String]
    , names :: [String]
    } deriving (Show, Eq, Generic)

instance FromJSON JsonFileType where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = unpack . replace "fileType" "type" . pack
              }

newtype JsonFileTypes
  = JsonFileTypes {filetypes :: [JsonFileType]}
  deriving (Show, Eq, Generic)

instance FromJSON JsonFileTypes

fileTypesJsonFile :: FilePath
fileTypesJsonFile = "filetypes.json"

getJsonFileTypes :: IO [JsonFileType]
getJsonFileTypes = do
  fileTypesJsonPath <- getDataFileName fileTypesJsonFile
  fileTypesJsonString <- getFileString fileTypesJsonPath
  case fileTypesJsonString of
    (Left _) -> return []
    (Right jsonString) ->
      case (eitherDecode (BC.pack jsonString) :: Either String JsonFileTypes) of
        (Left _) -> return []
        (Right jsonFileTypes) -> return (map normalizeType (filetypes jsonFileTypes))
  where normalizeType :: JsonFileType -> JsonFileType
        normalizeType ft = JsonFileType { fileType = fileType ft,
                                          extensions = map normalizeExtension (extensions ft) ,
                                          names = names ft }

getFileType :: FilePath -> IO FileType
getFileType f = do
  fileTypes <- getFileTypes [f]
  case fileTypes of
    [] -> return Unknown
    _  -> return $ head fileTypes

getFileTypes :: [FilePath] -> IO [FileType]
getFileTypes files = do
  jsonFileTypes <- getJsonFileTypes
  return $ map (fileTypeFromJsonFileTypes jsonFileTypes) files

fileTypeFromJsonFileTypes :: [JsonFileType] -> FilePath -> FileType
fileTypeFromJsonFileTypes jsonFileTypes fp =
  case matchingTypeForNameJson jsonFileTypes fileName of
    Unknown -> case getExtension fileName of
                  Just x -> matchingTypeForExtensionJson jsonFileTypes x
                  Nothing -> Unknown
    ft -> ft
  where fileName = takeFileName fp

matchingTypeForNameJson :: [JsonFileType] -> String -> FileType
matchingTypeForNameJson jsonFileTypes n =
  case filter (\f -> n `elem` names f) jsonFileTypes of
    [] -> Unknown
    fts -> case fileTypeName fts of
           "archive" -> Archive
           "audio" -> Audio
           "code" -> Code
           "font" -> Font
           "image" -> Image
           "video" -> Video
           "xml" -> Xml
           "binary" -> Binary
           tname | tname `elem` ["code", "text", "xml"] -> Text
           _ -> Unknown
  where fileTypeName = fileType . head

matchingTypeForExtensionJson :: [JsonFileType] -> String -> FileType
matchingTypeForExtensionJson jsonFileTypes x =
  case filter (\f -> x `elem` extensions f) jsonFileTypes of
    [] -> Unknown
    fts -> case fileTypeName fts of
           "archive" -> Archive
           "audio" -> Audio
           "code" -> Code
           "font" -> Font
           "image" -> Image
           "video" -> Video
           "xml" -> Xml
           "binary" -> Binary
           tname | tname `elem` ["code", "text", "xml"] -> Text
           _ -> Unknown
  where fileTypeName = fileType . head
