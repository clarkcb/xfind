module HsFind.FinderTest
  ( getFilterToFileResultTests
  , getIsMatchingArchiveFilePathTests
  , getIsMatchingDirPathTests
  , getIsMatchingFilePathTests
  , getFindPythonFileResultTests
  , getFindRubyFileResultTests
  , getFollowSymlinksTests
  ) where

import HsFind.Config (getXfindPath)
import HsFind.FileTypes
import HsFind.FindSettings
import HsFind.Finder

import Data.Maybe (isJust)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)


getIsMatchingDirPathTests :: IO [Test]
getIsMatchingDirPathTests = do
  let settings = defaultFindSettings
  let settingsInDirPattern = settings { inDirPatterns = ["hs"] }
  let settingsOutDirPattern = settings { outDirPatterns = ["cs"] }
  let settingsIncludeHidden = settings { includeHidden = True }
  let hsFindDir = "hsfind"
  let csFindDir = "csfind"
  return [ testCase "isMatchingDirPath hsfind default settings" (isMatchingDirPath settings hsFindDir @?= True)
         , testCase "isMatchingDirPath hsfind matching inDirPattern" (isMatchingDirPath settingsInDirPattern hsFindDir @?= True)
         , testCase "isMatchingDirPath hsfind not matching inDirPattern" (isMatchingDirPath settingsInDirPattern csFindDir @?= False)
         , testCase "isMatchingDirPath hsfind matching outDirPattern" (isMatchingDirPath settingsOutDirPattern csFindDir @?= False)
         , testCase "isMatchingDirPath hsfind not matching inDirPattern" (isMatchingDirPath settingsOutDirPattern hsFindDir @?= True)
         , testCase "isMatchingDirPath . default settings" (isMatchingDirPath settings "." @?= True)
         , testCase "isMatchingDirPath .. default settings" (isMatchingDirPath settings ".." @?= True)
         , testCase "isMatchingDirPath .git default settings" (isMatchingDirPath settings ".git" @?= False)
         , testCase "isMatchingDirPath .git includeHidden" (isMatchingDirPath settingsIncludeHidden ".git" @?= True)
         ]

getIsMatchingFilePathTests :: IO [Test]
getIsMatchingFilePathTests = do
  let settings = defaultFindSettings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let settingsOutExtension = settings { outExtensions = [".cs"] }
  let settingsInFilePattern = settings { inFilePatterns = ["Find"] }
  let settingsOutFilePattern = settings { outFilePatterns = ["Main"] }
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderHsFile = "Finder.hs"
  let finderCsFile = "Finder.cs"
  let mainHsFile = "Main.hs"
  let gitIgnoreFile = ".gitignore"
  return [ testCase "isMatchingFilePath Finder.hs default settings" (isMatchingFilePath settings finderHsFile @?= True)
         , testCase "isMatchingFilePath Finder.hs matching inExtensions" (isMatchingFilePath settingsInExtension finderHsFile @?= True)
         , testCase "isMatchingFilePath Finder.hs not matching inExtensions" (isMatchingFilePath settingsInExtension finderCsFile @?= False)
         , testCase "isMatchingFilePath Finder.hs matching outExtensions" (isMatchingFilePath settingsOutExtension finderCsFile @?= False)
         , testCase "isMatchingFilePath Finder.hs not matching outExtensions" (isMatchingFilePath settingsOutExtension finderHsFile @?= True)
         , testCase "isMatchingFilePath Finder.hs matching inFilePatterns" (isMatchingFilePath settingsInFilePattern finderHsFile @?= True)
         , testCase "isMatchingFilePath Main.hs not matching inFilePatterns" (isMatchingFilePath settingsInFilePattern mainHsFile @?= False)
         , testCase "isMatchingFilePath Main.hs matching outFilePatterns" (isMatchingFilePath settingsOutFilePattern mainHsFile @?= False)
         , testCase "isMatchingFilePath Finder.hs not matching outFilePatterns" (isMatchingFilePath settingsOutFilePattern finderHsFile @?= True)
         , testCase "isMatchingFilePath .gitignore default settings" (isMatchingFilePath settings gitIgnoreFile @?= False)
         , testCase "isMatchingFilePath .gitignore includeHidden" (isMatchingFilePath settingsIncludeHidden gitIgnoreFile @?= True)
         ]

getIsMatchingArchiveFilePathTests :: IO [Test]
getIsMatchingArchiveFilePathTests = do
  let settings = defaultFindSettings
  let settingsInArchiveExtension = settings { inArchiveExtensions = [".zip"] }
  let settingsOutArchiveExtension = settings { outArchiveExtensions = [".gz"] }
  let settingsInArchiveFilePattern = settings { inArchiveFilePatterns = ["arch"] }
  let settingsOutArchiveFilePattern = settings { outArchiveFilePatterns = ["comp"] }
  let settingsIncludeHidden = settings { includeHidden = True }
  let archiveZipFile = "archive.zip"
  let archiveTarGzFile = "archive.tar.gz"
  let compressedZipFile = "compressed.zip"
  let hiddenArchiveZipFile = ".gitarchive.zip"
  return [ testCase "isMatchingArchiveFilePath archive.zip default settings" (isMatchingArchiveFilePath settings archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath archive.zip matching inArchiveExtensions" (isMatchingArchiveFilePath settingsInArchiveExtension archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath archive.tar.gz not matching inArchiveExtensions" (isMatchingArchiveFilePath settingsInArchiveExtension archiveTarGzFile @?= False)
         , testCase "isMatchingArchiveFilePath archive.tar.gz matching outArchiveExtensions" (isMatchingArchiveFilePath settingsOutArchiveExtension archiveTarGzFile @?= False)
         , testCase "isMatchingArchiveFilePath archive.zip not matching outArchiveExtensions" (isMatchingArchiveFilePath settingsOutArchiveExtension archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath archive.zip matching inArchiveFilePatterns" (isMatchingArchiveFilePath settingsInArchiveFilePattern archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath compressed.zip not matching inArchiveFilePatterns" (isMatchingArchiveFilePath settingsInArchiveFilePattern compressedZipFile @?= False)
         , testCase "isMatchingArchiveFilePath compressed.zip matching outArchiveFilePatterns" (isMatchingArchiveFilePath settingsOutArchiveFilePattern compressedZipFile @?= False)
         , testCase "isMatchingArchiveFilePath archive.zip not matching outArchiveFilePatterns" (isMatchingArchiveFilePath settingsOutArchiveFilePattern archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath .gitarchive.zip default settings" (isMatchingArchiveFilePath settings hiddenArchiveZipFile @?= False)
         , testCase "isMatchingArchiveFilePath .gitarchive.zip includeHidden" (isMatchingArchiveFilePath settingsIncludeHidden hiddenArchiveZipFile @?= True)
         ]

getFilterToFileResultTests :: IO [Test]
getFilterToFileResultTests = do
  let settings = defaultFindSettings
  jsonFileTypes <- getJsonFileTypes
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let settingsOutExtension = settings { outExtensions = [".hs"] }
  let settingsIncludeHidden = settings { includeHidden = True }
  let settingsIncludeArchives = settings { includeArchives = True }
  let settingsArchivesOnly = settingsIncludeArchives { archivesOnly = True }
  let finderHsFile = ("Finder.hs", Code)
  let gitignoreFile = (".gitignore", Text)
  let archiveZipFile = ("archive.zip", Archive)
  return [ testCase "filterToFileResult Finder.hs default settings" (isJust (filterToFileResult settings jsonFileTypes finderHsFile) @?= True)
         , testCase "filterToFileResult Finder.hs isMatchingFilePath" (isJust (filterToFileResult settingsInExtension jsonFileTypes finderHsFile) @?= True)
         , testCase "filterToFileResult Finder.hs not isMatchingFilePath" (isJust (filterToFileResult settingsOutExtension jsonFileTypes finderHsFile) @?= False)
         , testCase "filterToFileResult .gitignore default settings" (isJust (filterToFileResult settings jsonFileTypes gitignoreFile) @?= False)
         , testCase "filterToFileResult .gitignore includeHidden" (isJust (filterToFileResult settingsIncludeHidden jsonFileTypes gitignoreFile) @?= True)
         , testCase "filterToFileResult archive.zip default settings" (isJust (filterToFileResult settings jsonFileTypes archiveZipFile) @?= False)
         , testCase "filterToFileResult archive.zip includeArchives" (isJust (filterToFileResult settingsIncludeArchives jsonFileTypes archiveZipFile) @?= True)
         , testCase "filterToFileResult archive.zip archivesOnly" (isJust (filterToFileResult settingsArchivesOnly jsonFileTypes archiveZipFile) @?= True)
         , testCase "filterToFileResult Finder.hs archivesOnly" (isJust (filterToFileResult settingsArchivesOnly jsonFileTypes finderHsFile) @?= False)
         ]

-- hsfind -D build -D cmake -D node_modules -D vendor -D venv -t audio /Users/cary/src/xfind/python --debug
getFindPythonFileResultTests :: IO [Test]
getFindPythonFileResultTests = do
  let settings = defaultFindSettings {
    debug = True,
    inFileTypes = [Audio],
    outFilePatterns = ["build", "cmake", "node_modules", "pycache", "vendor", "venv"],
    paths = ["/Users/cary/src/xfind/python"]
  }
  fileResults <- doFind settings
  return [ testCase "getFindPythonFileResultTests" (length fileResults @?= 0)
         ]

-- hsfind -D build -D cmake -D node_modules -D vendor -D venv -t audio /Users/cary/src/xfind/ruby --debug
getFindRubyFileResultTests :: IO [Test]
getFindRubyFileResultTests = do
  let settings = defaultFindSettings {
    debug = True,
    inFileTypes = [Audio],
    outFilePatterns = ["build", "cmake", "node_modules", "pycache", "vendor", "venv"],
    paths = ["/Users/cary/src/xfind/ruby"]
  }
  fileResults <- doFind settings
  return [ testCase "getFindRubyFileResultTests" (length fileResults @?= 0)
         ]

getFollowSymlinksTests :: IO [Test]
getFollowSymlinksTests = do
  xfindPath <- getXfindPath
  let defaultSettings = defaultFindSettings {
    paths = [xfindPath ++ "/bin"]
  }
  let followSymlinksSettings = defaultSettings { followSymlinks = True }
  let noFollowSymlinksSettings = defaultSettings { followSymlinks = False }
  defaultFileResults <- doFind defaultSettings
  followSymlinksFileResults <- doFind followSymlinksSettings
  noFollowSymlinksFileResults <- doFind noFollowSymlinksSettings
  return [ testCase "getFollowSymlinksTests defaultSettings" ((length defaultFileResults < 3) @?= True)
         , testCase "getFollowSymlinksTests followSymlinks" ((length followSymlinksFileResults > 2) @?= True)
         , testCase "getFollowSymlinksTests noFollowSymlinks" ((length noFollowSymlinksFileResults < 3) @?= True)
         ]
