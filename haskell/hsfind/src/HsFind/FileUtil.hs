{-# LANGUAGE ScopedTypeVariables #-}

module HsFind.FileUtil
    (
      expandPath
    , filterDirectories
    , filterFiles
    , filterOutSymlinks
    , getDirectoryFiles
    , getExtension
    , getExtensionIndex
    , getFileByteString
    , getFileLines
    , getFileSizes
    , getFileString
    , getModificationTimes
    , getNonDotDirectoryContents
    , getParentPath
    , getRecursiveContents
    , getRecursiveDirectories
    , hasExtension
    , isDirectory
    , isDotDir
    , isFile
    , isHiddenName
    , isHiddenFilePath
    , normalizeExtension
    , partitionDirsAndFiles
    , partitionExisting
    , pathExists
    , pathsExist
    ) where

import Control.Exception (IOException, handle)
import Control.Monad (filterM, forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.List (elemIndices, isPrefixOf)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, getFileSize, getModificationTime, pathIsSymbolicLink)
import System.FilePath ((</>), dropFileName, splitPath, takeFileName)
import System.IO (hSetNewlineMode, IOMode(..), universalNewlineMode, withFile)
import Data.Time (UTCTime)

import HsFind.Config (getHome)

expandPath :: FilePath -> IO FilePath
expandPath filePath = do
  if "~" `isPrefixOf` filePath
    then do
      userPath <- getHome
      let homePath = getParentPath userPath
      case filePath of
        "~" -> return userPath
        "~/" -> return userPath
        ('~':'/':xs) -> return $ userPath ++ "/" ++ xs
        _ -> return $ homePath ++ drop 1 filePath
    else return filePath

getExtensionIndex :: FilePath -> Int
getExtensionIndex fp = case elemIndices '.' fileName of
                          [] -> -1
                          idxs -> maximum idxs
  where fileName = takeFileName fp

getExtension :: FilePath -> Maybe String
getExtension "" = Nothing
getExtension fp = case maxDotIndex of
                  i | i < 1 -> Nothing
                  i | i == lastIndex fileName -> Nothing
                  _ -> Just $ trimToExt fileName
  where fileName = takeFileName fp
        maxDotIndex = getExtensionIndex fileName
        lastIndex name = length name - 1
        trimToExt = drop (maxDotIndex + 1)

normalizeExtension :: String -> String
normalizeExtension x = case x of
                       ('.':_) -> drop 1 $ lower x
                       _ -> lower x
  where lower = map toLower

getParentPath :: FilePath -> FilePath
getParentPath = dropFileName

hasExtension :: String -> String -> Bool
hasExtension fp x = getExtension fp == (Just . normalizeExtension) x

filterDirectories :: [FilePath] -> IO [FilePath]
filterDirectories = filterM doesDirectoryExist

filterFiles :: [FilePath] -> IO [FilePath]
filterFiles = filterM doesFileExist

filterOutSymlinks :: [FilePath] -> IO [FilePath]
filterOutSymlinks filePaths = do
  filterM (\f -> do
             isSymLink <- pathIsSymbolicLink f
             return $ not isSymLink) filePaths

partitionDirsAndFiles :: [FilePath] -> IO ([FilePath], [FilePath])
partitionDirsAndFiles filePaths = do
  dirs <- filterDirectories filePaths
  files <- filterFiles filePaths
  return (dirs, files)

partitionExisting :: [FilePath] -> IO ([FilePath], [FilePath])
partitionExisting filePaths = do
  existingPaths <- filterM pathExists filePaths
  let nonExistingPaths = filter (`notElem` existingPaths) filePaths
  return (existingPaths, nonExistingPaths)

isHiddenName :: String -> Bool
isHiddenName n = isPrefixOf "." n && notElem n dotDirs

isHiddenFilePath :: FilePath -> Bool
isHiddenFilePath f = any isHiddenName $ splitPath f

getDirectoryFiles :: FilePath -> IO [FilePath]
getDirectoryFiles dir = do
  names <- listDirectory dir
  filterFiles names

getFileSizes :: [FilePath] -> IO [Integer]
getFileSizes files = do
  mapM getFileSize files

getModificationTimes :: [FilePath] -> IO [UTCTime]
getModificationTimes files = do
  mapM getModificationTime files

getNonDotDirectoryContents :: FilePath -> IO [FilePath]
getNonDotDirectoryContents dir = do
  names <- listDirectory dir
  return $ map (dir </>) names

dotDirs :: [FilePath]
dotDirs = [".", "./", "..", "../"]

isDotDir :: FilePath -> Bool
isDotDir dir = dir `elem` dotDirs

isDirectory :: FilePath -> IO Bool
isDirectory = doesDirectoryExist

isFile :: FilePath -> IO Bool
isFile = doesFileExist

pathExists :: FilePath -> IO Bool
pathExists fp = do
  foundDir <- doesDirectoryExist fp
  if foundDir
    then return True
    else doesFileExist fp

pathsExist :: [FilePath] -> IO Bool
pathsExist paths = do
  foundPaths <- filterM pathExists paths
  if length foundPaths == length paths then do
    return True
  else do
    let notFoundPaths = filter (`notElem` foundPaths) paths
    expandedPaths <- mapM expandPath notFoundPaths
    foundExpandedPaths <- filterM pathExists expandedPaths
    let allFoundPaths = foundPaths ++ foundExpandedPaths
    if length allFoundPaths == length paths then do
      return True
    else do
      return False

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir = do
  filePaths <- getNonDotDirectoryContents dir
  paths <- forM filePaths $ \path -> do
    isDir <- doesDirectoryExist path
    if isDir
      then do
        subNames <- getRecursiveContents path
        return $ path : subNames
    else return [path]
  return (concat paths)

getRecursiveDirectories :: FilePath -> IO [FilePath]
getRecursiveDirectories dir = do
  filePaths <- getNonDotDirectoryContents dir
  paths <- forM filePaths $ \path -> do
    isDir <- doesDirectoryExist path
    if isDir
      then do
        subNames <- getRecursiveDirectories path
        return $ path : subNames
    else return []
  return (concat paths)

getFileByteString :: FilePath -> IO (Either String B.ByteString)
getFileByteString f = handle (\(e :: IOException) -> return (Left (show e))) $
  withFile f ReadMode $ \h -> do
    hSetNewlineMode h universalNewlineMode
    contents <- B.hGetContents h
    return (Right contents)

getFileString :: FilePath -> IO (Either String String)
getFileString f = do
  bsEither <- getFileByteString f
  case bsEither of
    (Left err) -> return $ Left err
    (Right bs) -> return $ Right (BC.unpack bs)

getFileLines :: FilePath -> IO (Either String [B.ByteString])
getFileLines f = do
  fileByteString <- getFileByteString f
  case fileByteString of
    Left e -> return $ Left e
    Right contents -> return $ Right (BC.split '\n' contents)
