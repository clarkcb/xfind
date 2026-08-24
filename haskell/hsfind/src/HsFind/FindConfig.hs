module HsFind.FindConfig
  ( FindConfig(..)
  , getFindConfig
  , getXfindPath
  , getDataPath
  , getDefaultFindSettingsPath
  ) where

import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

data FindConfig = FindConfig {
    xfindPath :: FilePath
  , fileTypesPath :: FilePath
  , findOptionsPath :: FilePath
  , defaultFindSettingsPath :: FilePath
  } deriving (Show, Eq)

getFindConfig :: IO FindConfig
getFindConfig = do
  xfindPath <- getXfindPath
  dataPath <- getDataPath
  let fileTypesPath = concatPath dataPath "filetypes.json"
  let findOptionsPath = concatPath dataPath "findoptions.json"
  defaultFindSettingsPath <- getDefaultFindSettingsPath
  return FindConfig {
    xfindPath=xfindPath
  , fileTypesPath=fileTypesPath
  , findOptionsPath=findOptionsPath
  , defaultFindSettingsPath=defaultFindSettingsPath
  }

concatPath :: FilePath -> FilePath -> FilePath
concatPath fp1 fp2 = fp1 </> fp2

getXfindPath :: IO FilePath
getXfindPath = do
  home <- getHomeDirectory
  maybeXfindPath <- lookupEnv "XFIND_PATH"
  case maybeXfindPath of
    Just xfindPath -> return xfindPath
    Nothing -> return $ foldl concatPath home ["src", "xfind"]

getDataPath :: IO FilePath
getDataPath = do
  xfindPath <- getXfindPath
  let elems = ["haskell", "hsfind", "data"]
  return $ foldl concatPath xfindPath elems

getDefaultFindSettingsPath :: IO FilePath
getDefaultFindSettingsPath = do
  home <- getHomeDirectory
  let elems = [".config", "xfind", "settings.json"]
  return $ foldl concatPath home elems
