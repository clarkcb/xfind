module HsFind.FinderTest
  ( getFilterFileTests
  , getIsArchiveFindFileTests
  , getIsFindDirTests
  , getIsFindFileTests
  ) where

import HsFind.Config
import HsFind.FileTypes
import HsFind.FileUtil
import HsFind.Finder
import HsFind.FindFile
import HsFind.FindSettings

import qualified Data.ByteString as B
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getIsFindDirTests :: IO [Test]
getIsFindDirTests = do
  let settings = defaultFindSettings
  let settingsInDirPattern = settings { inDirPatterns = ["hsfind"] }
  let settingsOutDirPattern = settings { outDirPatterns = ["csfind"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isFindDir hsfind default settings" (isFindDir settings "hsfind" @?= True)
         , testCase "isFindDir hsfind matching inDirPattern" (isFindDir settingsInDirPattern "hsfind" @?= True)
         , testCase "isFindDir hsfind not matching inDirPattern" (isFindDir settingsInDirPattern "csfind" @?= False)
         , testCase "isFindDir hsfind matching outDirPattern" (isFindDir settingsOutDirPattern "csfind" @?= False)
         , testCase "isFindDir hsfind not matching inDirPattern" (isFindDir settingsOutDirPattern "hsfind" @?= True)
         , testCase "isFindDir . default settings" (isFindDir settings "." @?= True)
         , testCase "isFindDir .. default settings" (isFindDir settings ".." @?= True)
         , testCase "isFindDir .git default settings" (isFindDir settings ".git" @?= False)
         , testCase "isFindDir .git includeHidden" (isFindDir settingsIncludeHidden ".git" @?= True)
         ]

getIsFindFileTests :: IO [Test]
getIsFindFileTests = do
  let settings = defaultFindSettings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let settingsOutExtension = settings { outExtensions = [".cs"] }
  let settingsInFilePattern = settings { inFilePatterns = ["Find"] }
  let settingsOutFilePattern = settings { outFilePatterns = ["Main"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isFindFile Finder.hs default settings" (isFindFile settings "Finder.hs" @?= True)
         , testCase "isFindFile Finder.hs matching inExtensions" (isFindFile settingsInExtension "Finder.hs" @?= True)
         , testCase "isFindFile Finder.hs not matching inExtensions" (isFindFile settingsInExtension "Finder.cs" @?= False)
         , testCase "isFindFile Finder.hs matching outExtensions" (isFindFile settingsOutExtension "Finder.cs" @?= False)
         , testCase "isFindFile Finder.hs not matching outExtensions" (isFindFile settingsOutExtension "Finder.hs" @?= True)
         , testCase "isFindFile Finder.hs matching inFilePatterns" (isFindFile settingsInFilePattern "Finder.hs" @?= True)
         , testCase "isFindFile Main.hs not matching inFilePatterns" (isFindFile settingsInFilePattern "Main.hs" @?= False)
         , testCase "isFindFile Main.hs matching outFilePatterns" (isFindFile settingsOutFilePattern "Main.hs" @?= False)
         , testCase "isFindFile Finder.hs not matching outFilePatterns" (isFindFile settingsOutFilePattern "Finder.hs" @?= True)
         , testCase "isFindFile .gitignore default settings" (isFindFile settings ".gitignore" @?= False)
         , testCase "isFindFile .gitignore includeHidden" (isFindFile settingsIncludeHidden ".gitignore" @?= True)
         ]

getIsArchiveFindFileTests :: IO [Test]
getIsArchiveFindFileTests = do
  let settings = defaultFindSettings
  let settingsInArchiveExtension = settings { inArchiveExtensions = [".zip"] }
  let settingsOutArchiveExtension = settings { outArchiveExtensions = [".gz"] }
  let settingsInArchiveFilePattern = settings { inArchiveFilePatterns = ["arch"] }
  let settingsOutArchiveFilePattern = settings { outArchiveFilePatterns = ["comp"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  return [ testCase "isArchiveFindFile archive.zip default settings" (isArchiveFindFile settings "archive.zip" @?= True)
         , testCase "isArchiveFindFile archive.zip matching inArchiveExtensions" (isArchiveFindFile settingsInArchiveExtension "archive.zip" @?= True)
         , testCase "isArchiveFindFile archive.tar.gz not matching inArchiveExtensions" (isArchiveFindFile settingsInArchiveExtension "archive.tar.gz" @?= False)
         , testCase "isArchiveFindFile archive.tar.gz matching outArchiveExtensions" (isArchiveFindFile settingsOutArchiveExtension "archive.tar.gz" @?= False)
         , testCase "isArchiveFindFile archive.zip not matching outArchiveExtensions" (isArchiveFindFile settingsOutArchiveExtension "archive.zip" @?= True)
         , testCase "isArchiveFindFile archive.zip matching inArchiveFilePatterns" (isArchiveFindFile settingsInArchiveFilePattern "archive.zip" @?= True)
         , testCase "isArchiveFindFile compressed.zip not matching inArchiveFilePatterns" (isArchiveFindFile settingsInArchiveFilePattern "compressed.zip" @?= False)
         , testCase "isArchiveFindFile compressed.zip matching outArchiveFilePatterns" (isArchiveFindFile settingsOutArchiveFilePattern "compressed.zip" @?= False)
         , testCase "isArchiveFindFile archive.zip not matching outArchiveFilePatterns" (isArchiveFindFile settingsOutArchiveFilePattern "archive.zip" @?= True)
         , testCase "isArchiveFindFile .gitarchive.zip default settings" (isArchiveFindFile settings ".gitarchive.zip" @?= False)
         , testCase "isArchiveFindFile .gitarchive.zip includeHidden" (isArchiveFindFile settingsIncludeHidden ".gitarchive.zip" @?= True)
         ]

getFilterFileTests :: IO [Test]
getFilterFileTests = do
  let settings = defaultFindSettings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let settingsOutExtension = settings { outExtensions = [".hs"] }
  let settingsIncludeHidden = settings { excludeHidden = False }
  let settingsIncludeArchives = settings { includeArchives = True }
  let settingsArchivesOnly = settingsIncludeArchives { archivesOnly = True }
  let hsTestFile = FindFile { findFileContainers=[], findFilePath="Finder.hs", findFileType=Text }
  let hiddenTestFile = FindFile { findFileContainers=[], findFilePath=".gitignore", findFileType=Text }
  let archiveTestFile = FindFile { findFileContainers=[], findFilePath="archive.zip", findFileType=Archive }
  return [ testCase "filterFile Finder.hs default settings" (filterFile settings hsTestFile @?= True)
         , testCase "filterFile Finder.hs isFindFile" (filterFile settingsInExtension hsTestFile @?= True)
         , testCase "filterFile Finder.hs not isFindFile" (filterFile settingsOutExtension hsTestFile @?= False)
         , testCase "filterFile .gitignore default settings" (filterFile settings hiddenTestFile @?= False)
         , testCase "filterFile .gitignore includeHidden" (filterFile settingsIncludeHidden hiddenTestFile @?= True)
         , testCase "filterFile archive.zip default settings" (filterFile settings archiveTestFile @?= False)
         , testCase "filterFile archive.zip includeArchives" (filterFile settingsIncludeArchives archiveTestFile @?= True)
         , testCase "filterFile archive.zip archivesOnly" (filterFile settingsArchivesOnly archiveTestFile @?= True)
         , testCase "filterFile Finder.hs archivesOnly" (filterFile settingsArchivesOnly hsTestFile @?= False)
         ]
