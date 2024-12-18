module Main (main) where

import HsFind.FileResultTest
import HsFind.FileTypesTest
import HsFind.FileUtilTest
import HsFind.FinderTest
import HsFind.FindOptionsTest
import HsFind.FindSettingsTest

import Test.Framework

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
  isMatchingDirPathTests <- getIsMatchingDirPathTests
  isMatchingFilePathTests <- getIsMatchingFilePathTests
  isMatchingArchiveFilePathTests <- getIsMatchingArchiveFilePathTests
  -- filterFileTests <- getFilterFileTests
  findRubyFileResultTests <- getFindRubyFileResultTests
  findPythonFileResultTests <- getFindPythonFileResultTests
  followSymlinksDefaultTests <- getFollowSymlinksDefaultTests
  followSymlinksTrueTests <- getFollowSymlinksTrueTests
  followSymlinksFalseTests <- getFollowSymlinksFalseTests

  -- FindOptions tests
  settingsFromArgsTests <- getSettingsFromArgsTests
  settingsFromNoArgsTests <- getSettingsFromNoArgsTests
  archivesOnlyTests <- getArchivesOnlyTests
  debugTests <- getDebugTests

  -- FindSettings tests
  defaultFindSettingsTests <- getDefaultFindSettingsTests
  newExtensionsTests <- getNewExtensionsTests

  defaultMain (fileResultTests ++ fileResultWithSizeTests ++
    fileTypeTests ++ fileTypeFromNameTests ++
    fileUtilTests ++ 
    isMatchingDirPathTests ++ isMatchingFilePathTests ++
    isMatchingArchiveFilePathTests ++ findPythonFileResultTests ++
    findRubyFileResultTests ++
    settingsFromArgsTests ++ settingsFromNoArgsTests ++
    archivesOnlyTests ++ debugTests ++
    followSymlinksDefaultTests ++ followSymlinksTrueTests ++ followSymlinksFalseTests ++
    defaultFindSettingsTests ++ newExtensionsTests)
