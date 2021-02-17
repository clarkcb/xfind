module HsFind.Paths_hsfind where

import System.FilePath ((</>))
import HsFind.Config (getDataPath)

-- NOTE: this path is only used for testing/development, after cabal install
-- the path will be overridden by the data-files setting in the cabal file
getDataFileName :: FilePath -> IO FilePath
getDataFileName f = do
  dataPath <- getDataPath
  return $ dataPath </> f
