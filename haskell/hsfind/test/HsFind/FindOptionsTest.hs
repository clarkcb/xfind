module HsFind.FindOptionsTest
  ( getArchivesOnlyTests
  , getDebugTests
  , getSettingsFromArgsTests
  , getSettingsFromNoArgsTests
  ) where

import HsFind.Finder
import HsFind.FindOptions
import HsFind.FindSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getSettingsFromNoArgsTests :: IO [Test]
getSettingsFromNoArgsTests = do
  findOptions <- getFindOptions
  --let settings = settingsFromArgs findOptions []
  case settingsFromArgs findOptions [] of
    Left errMsg -> return []
    Right settings ->
      return [ testCase "archivesOnly" (archivesOnly settings @?= False)
             , testCase "debug" (debug settings @?= False)
             , testCase "excludeHidden" (excludeHidden settings @?= True)
             , testCase "firstMatch" (firstMatch settings @?= False)
             , testCase "linesAfter" (linesAfter settings @?= 0)
             , testCase "linesBefore" (linesBefore settings @?= 0)
             , testCase "listDirs" (listDirs settings @?= False)
             , testCase "listFiles" (listFiles settings @?= False)
             , testCase "listLines" (listLines settings @?= False)
             , testCase "multiLineFind" (multiLineFind settings @?= False)
             , testCase "printResults" (printResults settings @?= True)
             , testCase "printUsage" (printUsage settings @?= False)
             , testCase "printVersion" (printVersion settings @?= False)
             , testCase "recursive" (recursive settings @?= True)
             , testCase "findArchives" (findArchives settings @?= False)
             , testCase "uniqueLines" (uniqueLines settings @?= False)
             , testCase "verbose" (verbose settings @?= False)
             ]

getSettingsFromArgsTests :: IO [Test]
getSettingsFromArgsTests = do
  let args = ["-x","hs","-X","hi,o","-s","Finder","-b","2","-B","2","."]
  findOptions <- getFindOptions
  --let settings = settingsFromArgs findOptions args
  case settingsFromArgs findOptions args of
    Left errMsg -> return []
    Right settings ->
      return [ testCase "startpath ." (startPath settings @?= ".")
             , testCase "-s Finder" (findPatterns settings @?= ["Finder"])
             , testCase "-x hs" (inExtensions settings @?= [".hs"])
             , testCase "-X hi,o" (outExtensions settings @?= [".hi", ".o"])
             , testCase "linesAfter" (linesAfter settings @?= 2)
             , testCase "linesBefore" (linesBefore settings @?= 2)
             ]

getArchivesOnlyTests :: IO [Test]
getArchivesOnlyTests = do
  let args = ["--archivesonly"]
  findOptions <- getFindOptions
  --let settings = settingsFromArgs findOptions args
  case settingsFromArgs findOptions args of
    Left errMsg -> return []
    Right settings ->
      return [ testCase "archivesOnly" (archivesOnly settings @?= True)
             , testCase "findArchives" (findArchives settings @?= True)
             ]

getDebugTests :: IO [Test]
getDebugTests = do
  let args = ["--debug"]
  findOptions <- getFindOptions
  --let settings = settingsFromArgs findOptions args
  case settingsFromArgs findOptions args of
    Left errMsg -> return []
    Right settings ->
      return [ testCase "debug" (debug settings @?= True)
             , testCase "verbose" (verbose settings @?= True)
             ]
