module Main (main) where

import HsFind.FileTypesTest
import HsFind.FileUtilTest
import HsFind.FinderTest
import HsFind.FindOptionsTest
import HsFind.FindResultTest
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
  isFindDirTests <- getIsFindDirTests
  isFindFileTests <- getIsFindFileTests
  isArchiveFindFileTests <- getIsArchiveFindFileTests
  filterFileTests <- getFilterFileTests
  findLinesTests <- getFindLinesTests
  findContentsTests <- getFindContentsTests

  -- FindOptions tests
  settingsFromArgsTests <- getSettingsFromArgsTests
  settingsFromNoArgsTests <- getSettingsFromNoArgsTests
  archivesOnlyTests <- getArchivesOnlyTests
  debugTests <- getDebugTests

  -- FindResult tests
  binaryFileFindResultTests <- getBinaryFileFindResultTests
  singleLineFindResultTests <- getSingleLineFindResultTests
  multiLineFindResultTests <- getMultiLineFindResultTests

  -- FindSettings tests
  defaultFindSettingsTests <- getDefaultFindSettingsTests
  newExtensionsTests <- getNewExtensionsTests

  defaultMain (fileTypeTests ++ fileTypeFromNameTests ++ fileUtilTests ++
    isFindDirTests ++ isFindFileTests ++ isArchiveFindFileTests ++
    filterFileTests ++ findLinesTests ++ findContentsTests ++
    settingsFromArgsTests ++ settingsFromNoArgsTests ++
    archivesOnlyTests ++ debugTests ++
    binaryFileFindResultTests ++ singleLineFindResultTests ++
    multiLineFindResultTests ++ 
    defaultFindSettingsTests ++ newExtensionsTests)
