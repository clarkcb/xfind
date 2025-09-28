module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

import HsFind.FindOptions (getFindOptions, getUsage, ioSettingsFromArgs)
import HsFind.Finder (doFind, formatMatchingDirs, formatMatchingFiles, getFinder, validateFindSettings)
import HsFind.FindSettings (FindSettings(..), findSettingsToString)


logMsg :: String -> IO ()
logMsg = putStr

logErr :: String -> IO ()
logErr s = hPutStr stderr $ "ERROR: " ++ s

main :: IO ()
main = do
  args <- getArgs
  findOptionsEither <- getFindOptions
  case findOptionsEither of
    Left errMsg -> do
      logMsg "\n"
      logErr $ errMsg ++ "\n"
    Right findOptions -> do
      settingsFromArgsEither <- ioSettingsFromArgs findOptions args
      case settingsFromArgsEither of
        Left errMsg -> do
          logMsg "\n"
          logErr $ errMsg ++ "\n"
          logMsg $ "\n" ++ getUsage findOptions ++ "\n"
        Right settings -> do
          logMsg $ if debug settings
                  then findSettingsToString settings ++ "\n"
                  else ""
          case validateFindSettings settings of
            Just errMsg -> do
              logMsg "\n"
              logErr $ errMsg ++ "\n"
              logMsg $ "\n" ++ getUsage findOptions ++ "\n"
            Nothing -> do
              if printUsage settings
                then logMsg $ "\n" ++ getUsage findOptions ++ "\n"
                else do
                  let finder = getFinder settings
                  findResultsEither <- doFind finder
                  case findResultsEither of
                    Left errMsg -> do
                      logMsg "\n"
                      logErr $ errMsg ++ "\n"
                      logMsg $ "\n" ++ getUsage findOptions ++ "\n"
                    Right fileResults -> do
                      logMsg $ if printDirs settings
                                then formatMatchingDirs settings fileResults
                                else ""
                      logMsg $ if printFiles settings
                                then formatMatchingFiles settings fileResults
                                else ""
                      logMsg ""
