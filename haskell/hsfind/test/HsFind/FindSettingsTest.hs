module HsFind.FindSettingsTest
  ( getDefaultFindSettingsTests
  , getNewExtensionsTests
  ) where

import HsFind.FindSettings

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getDefaultFindSettingsTests :: IO [Test]
getDefaultFindSettingsTests = do
  let settings = defaultFindSettings
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

getNewExtensionsTests :: IO [Test]
getNewExtensionsTests =
  return [ testCase "hs" (newExtensions "hs" @?= [".hs"])
         , testCase "hs,py" (newExtensions "hs,py" @?= [".hs", ".py"])
         ]
