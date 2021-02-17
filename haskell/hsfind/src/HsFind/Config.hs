module HsFind.Config
  (
      getXfindPath
    , getDataPath
  ) where

import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Info (os)

isWin :: Bool
isWin = os == "mingw32"

getHome :: IO FilePath
getHome = getEnv homeName
  where homeName = if isWin then "HOMEPATH" else "HOME"

xfindPath :: FilePath
xfindPath = "/Users/cary/src/xfind"

getXfindPath :: IO FilePath
getXfindPath =
  return xfindPath

getDataPath :: IO FilePath
getDataPath = do
  let elems = ["haskell", "hsfind", "data"]
  return $ foldl concatPath xfindPath elems
  where concatPath path p = path </> p  
