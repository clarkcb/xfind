module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

import HsFind.ConsoleColor (boldRed, consoleReset)
import HsFind.FindOptions (getFindOptions, getUsage, settingsFromArgs)
import HsFind.Finder (doFind, formatMatchingDirs, formatMatchingFiles, getFinder, validateFindSettings)
import HsFind.FindSettings (FindSettings(..), findSettingsToString)


logMsg :: String -> IO ()
logMsg = putStr

logErr :: String -> IO ()
logErr s = hPutStr stderr $ "ERROR: " ++ s

logErrColor :: String -> Bool -> IO ()
logErrColor s colorize =
  if colorize
    then hPutStr stderr $ boldRed ++ "ERROR: " ++ s ++ consoleReset ++ "\n"
    else hPutStr stderr $ "ERROR: " ++ s ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  findOptionsEither <- getFindOptions
  case findOptionsEither of
    Left errMsg -> do
      logMsg "\n"
      logErr $ errMsg ++ "\n"
    Right findOptions -> do
      settingsFromArgsEither <- settingsFromArgs findOptions args
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
              logErrColor errMsg $ colorize settings
              logMsg $ "\n" ++ getUsage findOptions ++ "\n"
            Nothing -> do
              if printUsage settings
                then logMsg $ "\n" ++ getUsage findOptions ++ "\n"
                else do
                  findResultsEither <- doFind $ getFinder settings
                  case findResultsEither of
                    Left errMsg -> do
                      logMsg "\n"
                      logErrColor errMsg $ colorize settings
                      logMsg $ "\n" ++ getUsage findOptions ++ "\n"
                    Right fileResults -> do
                      logMsg $ if printDirs settings
                                then formatMatchingDirs settings fileResults
                                else ""
                      logMsg $ if printFiles settings
                                then formatMatchingFiles settings fileResults
                                else ""
                      logMsg ""
