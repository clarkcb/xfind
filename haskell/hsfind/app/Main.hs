module Main (main) where

import Data.List (nub, sort)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.IO (hPutStr, stderr)

import HsFind.FileResult (FileResult, fileResultPath, fileResultToString)
import HsFind.FindOptions (getFindOptions, getUsage, settingsFromArgs)
import HsFind.Finder (doFind, validateFindSettings)
import HsFind.FindSettings (FindSettings(..), findSettingsToString)


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
      case validateFindSettings settings of
        Just errMsg -> do
          logMsg "\n"
          logErr $ errMsg ++ "\n"
          logMsg $ getUsage findOptions ++ "\n"
        Nothing -> do
          if printUsage settings
            then logMsg $ getUsage findOptions ++ "\n"
            else do
              findResultsEither <- doFind settings
              case findResultsEither of
                Left errMsg -> do
                  logMsg "\n"
                  logErr $ errMsg ++ "\n"
                  logMsg "\n"
                  logMsg $ getUsage findOptions ++ "\n"
                Right fileResults -> do
                  logMsg $ if printDirs settings
                            then formatMatchingDirs fileResults
                            else ""
                  logMsg $ if printFiles settings
                            then formatMatchingFiles fileResults
                            else ""
                  logMsg ""
