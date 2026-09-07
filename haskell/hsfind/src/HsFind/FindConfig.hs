module HsFind.FindConfig
  ( FindConfig(..)
  , getFindConfig
  , getXfindPath
  , getFindDataPath
  , getDefaultFindSettingsPath
  ) where

import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)

import HsFind.FileUtil (concatPath)

data FindConfig = FindConfig {
    xfindPath :: FilePath
  , fileTypesPath :: FilePath
  , findOptionsPath :: FilePath
  , defaultFindSettingsPath :: FilePath
  } deriving (Show, Eq)

getFindConfig :: IO FindConfig
getFindConfig = do
  xfindPath <- getXfindPath
  dataPath <- getFindDataPath
  let fileTypesPath = concatPath dataPath "filetypes.json"
  let findOptionsPath = concatPath dataPath "findoptions.json"
  defaultFindSettingsPath <- getDefaultFindSettingsPath
  return FindConfig {
    xfindPath=xfindPath
  , fileTypesPath=fileTypesPath
  , findOptionsPath=findOptionsPath
  , defaultFindSettingsPath=defaultFindSettingsPath
  }

defaultXfindConfigDir :: IO FilePath
defaultXfindConfigDir = do
  home <- getHomeDirectory
  return $ foldl concatPath home [".config", "xfind"]

getXfindConfigDir :: IO FilePath
getXfindConfigDir = do
  home <- getHomeDirectory
  maybeXfindConfigDir <- lookupEnv "XFIND_CONFIG_DIR"
  case maybeXfindConfigDir of
    Just xfindConfigDir -> return xfindConfigDir
    Nothing -> defaultXfindConfigDir

defaultXfindPath :: IO FilePath
defaultXfindPath = do
  home <- getHomeDirectory
  return $ foldl concatPath home ["src", "xfind"]

getXfindPath :: IO FilePath
getXfindPath = do
  home <- getHomeDirectory
  maybeXfindPath <- lookupEnv "XFIND_PATH"
  case maybeXfindPath of
    Just xfindPath -> return xfindPath
    Nothing -> defaultXfindPath

getFindDataPath :: IO FilePath
getFindDataPath = do
  xfindPath <- getXfindPath
  let elems = ["haskell", "hsfind", "data"]
  return $ foldl concatPath xfindPath elems

getDefaultFindSettingsPath :: IO FilePath
getDefaultFindSettingsPath = do
  xfindConfigDir <- getXfindConfigDir
  let elems = ["settings.json"]
  return $ foldl concatPath xfindConfigDir elems
