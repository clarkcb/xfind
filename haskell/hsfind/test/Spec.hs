module Main (main) where

import HsFind.FileResultTest
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
  -- FileResult tests
  fileResultTests <- getFileResultTests
  fileResultWithSizeTests <- getFileResultWithSizeTests

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

  defaultMain (fileResultTests ++ fileResultWithSizeTests ++ fileTypeTests ++
    fileTypeFromNameTests ++ fileUtilTests ++ isMatchingDirTests ++ isMatchingFileTests ++
    isMatchingArchiveFileTests ++
    -- filterFileTests ++ settingsFromArgsTests ++ settingsFromNoArgsTests ++
    settingsFromArgsTests ++ settingsFromNoArgsTests ++
    archivesOnlyTests ++ debugTests ++
    defaultFindSettingsTests ++ newExtensionsTests)
