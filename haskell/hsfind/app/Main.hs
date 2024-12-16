module Main (main) where

import Control.Monad (filterM)
import Data.List (nub, sort)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.IO (hPutStr, stderr)

import HsFind.FileResult (FileResult, fileResultPath, fileResultToString)
import HsFind.FileUtil (expandPath, pathExists)
import HsFind.FindOptions (FindOption, getFindOptions, getUsage, settingsFromArgs)
import HsFind.Finder (doFind, validateSettings)
import HsFind.FindSettings (FindSettings(..), findSettingsToString)


errsOrUsage :: [FindOption] -> FindSettings -> Maybe String
errsOrUsage findOptions settings =
  case usage of
    "" -> Nothing
    _  -> Just usage
  where errs   = validateSettings settings
        errMsg = if not (null errs)
                 then "\nERROR: " ++ head errs ++ "\n\n"
                 else ""
        usage  = case (printUsage settings, not (null errMsg)) of
                   (True, _)     -> "\n" ++ getUsage findOptions
                   (False, True) -> errMsg ++ getUsage findOptions
                   _ -> ""

getMatchingDirs :: [FileResult] -> [FilePath]
getMatchingDirs = sort . nub . map (takeDirectory . fileResultPath)

formatMatchingDirs :: [FileResult] -> String
formatMatchingDirs fileResults =
  if not (null matchingDirs) then
    "\nMatching directories (" ++ show (length matchingDirs) ++ "):\n" ++
    unlines matchingDirs
  else "\nMatching directories: 0\n"
  where matchingDirs = getMatchingDirs fileResults

formatMatchingFiles :: [FileResult] -> String
formatMatchingFiles fileResults =
  if not (null matchingFiles) then
    "\nMatching files (" ++ show (length matchingFiles) ++ "):\n" ++
    unlines matchingFiles
  else "\nMatching files: 0\n"
  where matchingFiles = map fileResultToString fileResults

logMsg :: String -> IO ()
logMsg = putStr

logErr :: String -> IO ()
logErr s = hPutStr stderr $ "ERROR: " ++ s

main :: IO ()
main = do
  args <- getArgs
  findOptions <- getFindOptions
  case settingsFromArgs findOptions args of
    Left errMsg -> do
      logMsg "\n"
      logErr $ errMsg ++ "\n"
      logMsg $ getUsage findOptions ++ "\n"
    Right settings -> do
      logMsg $ if debug settings
               then findSettingsToString settings ++ "\n"
               else ""
      case errsOrUsage findOptions settings of
        Just usage -> logMsg $ usage ++ "\n"
        Nothing -> do
          foundPaths <- filterM pathExists (paths settings)
          let notFoundPaths = filter (`notElem` foundPaths) (paths settings)
          expandedPaths <- mapM expandPath notFoundPaths
          let allPaths = foundPaths ++ expandedPaths
          if length foundPaths == length (paths settings) then do
            fileResults <- doFind settings
            logMsg $ if printDirs settings
                     then formatMatchingDirs fileResults
                     else ""
            logMsg $ if printFiles settings
                     then formatMatchingFiles fileResults
                     else ""
            logMsg ""
          else do
            logMsg "\n"
            logErr "Startpath not found\n\n"
            logMsg $ getUsage findOptions ++ "\n"
