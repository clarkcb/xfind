module HsFind.FinderTest
  ( getFilterToFileResultTests
  , getIsMatchingArchiveFileTests
  , getIsMatchingDirTests
  , getIsMatchingFileTests
  ) where

import HsFind.Config
-- import HsFind.FileResult
import HsFind.FileTypes
import HsFind.FileUtil
import HsFind.FindSettings
import HsFind.Finder

import qualified Data.ByteString as B
import Data.Maybe (isJust)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)


getIsMatchingDirTests :: IO [Test]
getIsMatchingDirTests = do
  let settings = defaultFindSettings
  let settingsInDirPattern = settings { inDirPatterns = ["hsfind"] }
  let settingsOutDirPattern = settings { outDirPatterns = ["csfind"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isMatchingDir hsfind default settings" (isMatchingDir settings "hsfind" @?= True)
         , testCase "isMatchingDir hsfind matching inDirPattern" (isMatchingDir settingsInDirPattern "hsfind" @?= True)
         , testCase "isMatchingDir hsfind not matching inDirPattern" (isMatchingDir settingsInDirPattern "csfind" @?= False)
         , testCase "isMatchingDir hsfind matching outDirPattern" (isMatchingDir settingsOutDirPattern "csfind" @?= False)
         , testCase "isMatchingDir hsfind not matching inDirPattern" (isMatchingDir settingsOutDirPattern "hsfind" @?= True)
         , testCase "isMatchingDir . default settings" (isMatchingDir settings "." @?= True)
         , testCase "isMatchingDir .. default settings" (isMatchingDir settings ".." @?= True)
         , testCase "isMatchingDir .git default settings" (isMatchingDir settings ".git" @?= False)
         , testCase "isMatchingDir .git includeHidden" (isMatchingDir settingsIncludeHidden ".git" @?= True)
         ]

getIsMatchingFileTests :: IO [Test]
getIsMatchingFileTests = do
  let settings = defaultFindSettings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let settingsOutExtension = settings { outExtensions = [".cs"] }
  let settingsInFilePattern = settings { inFilePatterns = ["Find"] }
  let settingsOutFilePattern = settings { outFilePatterns = ["Main"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isMatchingFile Finder.hs default settings" (isMatchingFile settings "Finder.hs" @?= True)
         , testCase "isMatchingFile Finder.hs matching inExtensions" (isMatchingFile settingsInExtension "Finder.hs" @?= True)
         , testCase "isMatchingFile Finder.hs not matching inExtensions" (isMatchingFile settingsInExtension "Finder.cs" @?= False)
         , testCase "isMatchingFile Finder.hs matching outExtensions" (isMatchingFile settingsOutExtension "Finder.cs" @?= False)
         , testCase "isMatchingFile Finder.hs not matching outExtensions" (isMatchingFile settingsOutExtension "Finder.hs" @?= True)
         , testCase "isMatchingFile Finder.hs matching inFilePatterns" (isMatchingFile settingsInFilePattern "Finder.hs" @?= True)
         , testCase "isMatchingFile Main.hs not matching inFilePatterns" (isMatchingFile settingsInFilePattern "Main.hs" @?= False)
         , testCase "isMatchingFile Main.hs matching outFilePatterns" (isMatchingFile settingsOutFilePattern "Main.hs" @?= False)
         , testCase "isMatchingFile Finder.hs not matching outFilePatterns" (isMatchingFile settingsOutFilePattern "Finder.hs" @?= True)
         , testCase "isMatchingFile .gitignore default settings" (isMatchingFile settings ".gitignore" @?= False)
         , testCase "isMatchingFile .gitignore includeHidden" (isMatchingFile settingsIncludeHidden ".gitignore" @?= True)
         ]

getIsMatchingArchiveFileTests :: IO [Test]
getIsMatchingArchiveFileTests = do
  let settings = defaultFindSettings
  let settingsInArchiveExtension = settings { inArchiveExtensions = [".zip"] }
  let settingsOutArchiveExtension = settings { outArchiveExtensions = [".gz"] }
  let settingsInArchiveFilePattern = settings { inArchiveFilePatterns = ["arch"] }
  let settingsOutArchiveFilePattern = settings { outArchiveFilePatterns = ["comp"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isMatchingArchiveFile archive.zip default settings" (isMatchingArchiveFile settings "archive.zip" @?= True)
         , testCase "isMatchingArchiveFile archive.zip matching inArchiveExtensions" (isMatchingArchiveFile settingsInArchiveExtension "archive.zip" @?= True)
         , testCase "isMatchingArchiveFile archive.tar.gz not matching inArchiveExtensions" (isMatchingArchiveFile settingsInArchiveExtension "archive.tar.gz" @?= False)
         , testCase "isMatchingArchiveFile archive.tar.gz matching outArchiveExtensions" (isMatchingArchiveFile settingsOutArchiveExtension "archive.tar.gz" @?= False)
         , testCase "isMatchingArchiveFile archive.zip not matching outArchiveExtensions" (isMatchingArchiveFile settingsOutArchiveExtension "archive.zip" @?= True)
         , testCase "isMatchingArchiveFile archive.zip matching inArchiveFilePatterns" (isMatchingArchiveFile settingsInArchiveFilePattern "archive.zip" @?= True)
         , testCase "isMatchingArchiveFile compressed.zip not matching inArchiveFilePatterns" (isMatchingArchiveFile settingsInArchiveFilePattern "compressed.zip" @?= False)
         , testCase "isMatchingArchiveFile compressed.zip matching outArchiveFilePatterns" (isMatchingArchiveFile settingsOutArchiveFilePattern "compressed.zip" @?= False)
         , testCase "isMatchingArchiveFile archive.zip not matching outArchiveFilePatterns" (isMatchingArchiveFile settingsOutArchiveFilePattern "archive.zip" @?= True)
         , testCase "isMatchingArchiveFile .gitarchive.zip default settings" (isMatchingArchiveFile settings ".gitarchive.zip" @?= False)
         , testCase "isMatchingArchiveFile .gitarchive.zip includeHidden" (isMatchingArchiveFile settingsIncludeHidden ".gitarchive.zip" @?= True)
         ]

getFilterToFileResultTests :: IO [Test]
getFilterToFileResultTests = do
  let settings = defaultFindSettings
  jsonFileTypes <- getJsonFileTypes
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let settingsOutExtension = settings { outExtensions = [".hs"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  let settingsIncludeArchives = settings { includeArchives = True }
  let settingsArchivesOnly = settingsIncludeArchives { archivesOnly = True }
  -- let hsTestFileResult = FileResult { fileResultContainers=[], fileResultPath="Finder.hs", fileResultType=Text }
  let hsTestFile = ("Finder.hs", Code)
  -- let hsTestFileType = getFileType hsTestFile
  -- let hiddenTestFileResult = FileResult { fileResultContainers=[], fileResultPath=".gitignore", fileResultType=Text }
  let hiddenTestFile = (".gitignore", Text)
  -- let archiveTestFileResult = FileResult { fileResultContainers=[], fileResultPath="archive.zip", fileResultType=Archive }
  let archiveTestFile = ("archive.zip", Archive)
  return [ testCase "filterToFileResult Finder.hs default settings" (isJust (filterToFileResult settings jsonFileTypes hsTestFile) @?= True)
         , testCase "filterToFileResult Finder.hs isMatchingFile" (isJust (filterToFileResult settingsInExtension jsonFileTypes hsTestFile) @?= True)
         , testCase "filterToFileResult Finder.hs not isMatchingFile" (isJust (filterToFileResult settingsOutExtension jsonFileTypes hsTestFile) @?= False)
         , testCase "filterToFileResult .gitignore default settings" (isJust (filterToFileResult settings jsonFileTypes hiddenTestFile) @?= False)
         , testCase "filterToFileResult .gitignore includeHidden" (isJust (filterToFileResult settingsIncludeHidden jsonFileTypes hiddenTestFile) @?= True)
         , testCase "filterToFileResult archive.zip default settings" (isJust (filterToFileResult settings jsonFileTypes archiveTestFile) @?= False)
         , testCase "filterToFileResult archive.zip includeArchives" (isJust (filterToFileResult settingsIncludeArchives jsonFileTypes archiveTestFile) @?= True)
         , testCase "filterToFileResult archive.zip archivesOnly" (isJust (filterToFileResult settingsArchivesOnly jsonFileTypes archiveTestFile) @?= True)
         , testCase "filterToFileResult Finder.hs archivesOnly" (isJust (filterToFileResult settingsArchivesOnly jsonFileTypes hsTestFile) @?= False)
         ]
