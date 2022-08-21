module Main (main) where

import HsFind.FileTypesTest
import HsFind.FileUtilTest
import HsFind.FinderTest
import HsFind.FindOptionsTest
import HsFind.FindSettingsTest

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

main :: IO ()
main = do
  -- FileTypes tests
  fileTypeTests <- getFileTypeTests
  fileTypeFromNameTests <- getFileTypeFromNameTests

  -- FileUtil tests
  fileUtilTests <- getFileUtilTests

  -- Finder tests
  isMatchingDirTests <- getIsMatchingDirTests
  isMatchingFileTests <- getIsMatchingFileTests
  isMatchingArchiveFileTests <- getIsMatchingArchiveFileTests
  -- filterFileTests <- getFilterFileTests

  -- FindOptions tests
  settingsFromArgsTests <- getSettingsFromArgsTests
  settingsFromNoArgsTests <- getSettingsFromNoArgsTests
  archivesOnlyTests <- getArchivesOnlyTests
  debugTests <- getDebugTests

  -- FindSettings tests
  defaultFindSettingsTests <- getDefaultFindSettingsTests
  newExtensionsTests <- getNewExtensionsTests

  defaultMain (fileTypeTests ++ fileTypeFromNameTests ++ fileUtilTests ++
    isMatchingDirTests ++ isMatchingFileTests ++ isMatchingArchiveFileTests ++
    -- filterFileTests ++ settingsFromArgsTests ++ settingsFromNoArgsTests ++
    settingsFromArgsTests ++ settingsFromNoArgsTests ++
    archivesOnlyTests ++ debugTests ++
    defaultFindSettingsTests ++ newExtensionsTests)
