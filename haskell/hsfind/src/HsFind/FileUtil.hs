{-# LANGUAGE ScopedTypeVariables #-}

module HsFind.FileUtil
    (
      filterDirectories
    , filterFiles
    , getDirectoryFiles
    , getExtension
    , getFileByteString
    , getFileLines
    , getFileSizes
    , getFileString
    , getModificationTimes
    , getNonDotDirectoryContents
    , getParentPath
    , getRecursiveContents
    , getRecursiveFilteredContents
    , getRecursiveDirectories
    , hasExtension
    , isDirectory
    , isDotDir
    , isFile
    , isHiddenFilePath
    , normalizeExtension
    , pathExists
    ) where

import Control.Exception (IOException, handle)
import Control.Monad (filterM, forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.List (elemIndices, isPrefixOf)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, getFileSize, getModificationTime)
import System.FilePath ((</>), dropFileName, splitPath, takeFileName)
import System.IO (hSetNewlineMode, IOMode(..), universalNewlineMode, withFile)
import Data.Time (UTCTime)

-- preferring this over System.FilePath (takeExtension)
getExtension :: FilePath -> Maybe String
getExtension "" = Nothing
getExtension fp = case maxDotIndex (takeFileName fp) of
                  0 -> Nothing
                  i | i == lastIndex (takeFileName fp) -> Nothing
                  _ -> Just $ trimToExt (takeFileName fp)
  where maxDotIndex name = case elemIndices '.' name of
                           [] -> 0
                           idxs -> maximum idxs
        lastIndex name = length name - 1
        trimToExt name = drop (maxDotIndex name) name

normalizeExtension :: String -> String
normalizeExtension x = case x of
                       ('.':_) -> lower x
                       _ -> '.' : lower x
  where lower = map toLower

getParentPath :: FilePath -> FilePath
getParentPath = dropFileName

hasExtension :: String -> String -> Bool
hasExtension fp x = getExtension fp == (Just . normalizeExtension) x

filterDirectories :: [FilePath] -> IO [FilePath]
filterDirectories = filterM doesDirectoryExist

filterFiles :: [FilePath] -> IO [FilePath]
filterFiles = filterM doesFileExist

isHiddenFilePath :: FilePath -> Bool
isHiddenFilePath f = any isHidden pathElems
  where isHidden = isPrefixOf "."
        pathElems = filter (`notElem` dotDirs) $ splitPath f

getDirectoryFiles :: FilePath -> IO [FilePath]
getDirectoryFiles dir = do
  names <- getNonDotDirectoryContents dir
  filterFiles names

getFileSizes :: [FilePath] -> IO [Integer]
getFileSizes files = do
  mapM getFileSize files

getModificationTimes :: [FilePath] -> IO [UTCTime]
getModificationTimes files = do
  mapM getModificationTime files

getNonDotDirectoryContents :: FilePath -> IO [FilePath]
getNonDotDirectoryContents dir = do
  names <- getDirectoryContents dir
  let filteredNames = filter (not . isDotDir) names
  return $ map (dir </>) filteredNames

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

getRecursiveFilteredContents :: FilePath -> Integer -> Integer -> Integer -> (FilePath -> Bool) -> (FilePath -> Bool) -> IO [FilePath]
getRecursiveFilteredContents dir depth minDepth maxDepth dirFilter fileFilter = do
  filePaths <- getNonDotDirectoryContents dir
  paths <- forM filePaths $ \path -> do
    d <- doesDirectoryExist path
    f <- doesFileExist path
    case (d, f) of
      (True, False) ->
        if (maxDepth < 1 || depth <= maxDepth) && dirFilter path
          then getRecursiveFilteredContents path (depth + 1) minDepth maxDepth dirFilter fileFilter
        else return []
      (False, True) ->
        if depth >= minDepth && (maxDepth < 1 || depth <= maxDepth) && fileFilter (takeFileName path)
          then
            return [path]
        else return []
      (_, _) -> return []
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
