module Main (main) where

import HsSearch.FileTypesTest
import HsSearch.FileUtilTest
import HsSearch.SearcherTest
import HsSearch.SearchOptionsTest
import HsSearch.SearchResultTest
import HsSearch.SearchSettingsTest

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

  -- Searcher tests
  isSearchDirTests <- getIsSearchDirTests
  isSearchFileTests <- getIsSearchFileTests
  isArchiveSearchFileTests <- getIsArchiveSearchFileTests
  filterFileTests <- getFilterFileTests
  searchLinesTests <- getSearchLinesTests
  searchContentsTests <- getSearchContentsTests

  -- SearchOptions tests
  settingsFromArgsTests <- getSettingsFromArgsTests
  settingsFromNoArgsTests <- getSettingsFromNoArgsTests
  archivesOnlyTests <- getArchivesOnlyTests
  debugTests <- getDebugTests

  -- SearchResult tests
  binaryFileSearchResultTests <- getBinaryFileSearchResultTests
  singleLineSearchResultTests <- getSingleLineSearchResultTests
  multiLineSearchResultTests <- getMultiLineSearchResultTests

  -- SearchSettings tests
  defaultSearchSettingsTests <- getDefaultSearchSettingsTests
  newExtensionsTests <- getNewExtensionsTests

  defaultMain (fileTypeTests ++ fileTypeFromNameTests ++ fileUtilTests ++
    isSearchDirTests ++ isSearchFileTests ++ isArchiveSearchFileTests ++
    filterFileTests ++ searchLinesTests ++ searchContentsTests ++
    settingsFromArgsTests ++ settingsFromNoArgsTests ++
    archivesOnlyTests ++ debugTests ++
    binaryFileSearchResultTests ++ singleLineSearchResultTests ++
    multiLineSearchResultTests ++ 
    defaultSearchSettingsTests ++ newExtensionsTests)
