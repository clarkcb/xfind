module HsFind.FindOptionsTest
  ( getArchivesOnlyTests
  , getDebugTests
  , getSettingsFromArgsTests
  , getSettingsFromNoArgsTests
  ) where

import HsFind.FindOptions
import HsFind.FindSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getSettingsFromNoArgsTests :: IO [Test]
getSettingsFromNoArgsTests = do
  findOptions <- getFindOptions
  case settingsFromArgs findOptions [] of
    Left _ -> return []
    Right settings ->
      return [ testCase "archivesOnly" (archivesOnly settings @?= False)
             , testCase "debug" (debug settings @?= False)
             , testCase "followSymlinks" (followSymlinks settings @?= False)
             , testCase "includeArchives" (includeArchives settings @?= False)
             , testCase "includeHidden" (includeHidden settings @?= False)
             , testCase "printDirs" (printDirs settings @?= False)
             , testCase "printFiles" (printFiles settings @?= True)
             , testCase "printUsage" (printUsage settings @?= False)
             , testCase "printVersion" (printVersion settings @?= False)
             , testCase "recursive" (recursive settings @?= True)
             , testCase "verbose" (verbose settings @?= False)
             ]

getSettingsFromArgsTests :: IO [Test]
getSettingsFromArgsTests = do
  let args = ["-x","hs","-X","hi,o","."]
  findOptions <- getFindOptions
  case settingsFromArgs findOptions args of
    Left _ -> return []
    Right settings ->
      return [ testCase "paths ." (paths settings @?= ["."])
             , testCase "-x hs" (inExtensions settings @?= ["hs"])
             , testCase "-X hi,o" (outExtensions settings @?= ["hi", "o"])
             ]

getArchivesOnlyTests :: IO [Test]
getArchivesOnlyTests = do
  let args = ["--archivesonly"]
  findOptions <- getFindOptions
  case settingsFromArgs findOptions args of
    Left _ -> return []
    Right settings ->
      return [ testCase "archivesOnly" (archivesOnly settings @?= True)
             , testCase "includeArchives" (includeArchives settings @?= True)
             ]

getDebugTests :: IO [Test]
getDebugTests = do
  let args = ["--debug"]
  findOptions <- getFindOptions
  case settingsFromArgs findOptions args of
    Left _ -> return []
    Right settings ->
      return [ testCase "debug" (debug settings @?= True)
             , testCase "verbose" (verbose settings @?= True)
             ]
