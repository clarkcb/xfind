module HsFind.Config
  (
      getXfindPath
    , getDataPath
  ) where

import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))
import System.Info (os)

isWin :: Bool
isWin = os == "mingw32"

getHome :: IO FilePath
getHome = getEnv homeName
  where homeName = if isWin then "HOMEPATH" else "HOME"

getXfindPath :: IO FilePath
getXfindPath = do
  home <- getHome
  maybeXfindPath <- lookupEnv "XFIND_PATH"
  case maybeXfindPath of
    Just xfindPath -> return xfindPath
    Nothing -> return $ home ++ "/src/xfind"

getDataPath :: IO FilePath
getDataPath = do
  xfindPath <- getXfindPath
  let elems = ["haskell", "hsfind", "data"]
  return $ foldl concatPath xfindPath elems
  where concatPath path p = path </> p  
