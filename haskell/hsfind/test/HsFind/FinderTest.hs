module HsFind.FinderTest
  ( getFilterToFileResultTests
  , getIsMatchingArchiveFilePathTests
  , getIsMatchingDirPathTests
  , getIsMatchingFilePathTests
  , getFindPythonFileResultTests
  , getFindRubyFileResultTests
  , getFollowSymlinksDefaultTests
  , getFollowSymlinksTrueTests
  , getFollowSymlinksFalseTests
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
  let finder = getFinder settings
  let settingsInDirPattern = settings { inDirPatterns = ["hs"] }
  let finderSettingsInDirPattern = getFinder settingsInDirPattern
  let settingsOutDirPattern = settings { outDirPatterns = ["cs"] }
  let finderSettingsOutDirPattern = getFinder settingsOutDirPattern
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderSettingsIncludeHidden = getFinder settingsIncludeHidden
  let hsFindDir = "hsfind"
  let csFindDir = "csfind"
  return [ testCase "isMatchingDirPath hsfind default settings" (isMatchingDirPath finder hsFindDir @?= True)
         , testCase "isMatchingDirPath hsfind matching inDirPattern" (isMatchingDirPath finderSettingsInDirPattern hsFindDir @?= True)
         , testCase "isMatchingDirPath hsfind not matching inDirPattern" (isMatchingDirPath finderSettingsInDirPattern csFindDir @?= False)
         , testCase "isMatchingDirPath hsfind matching outDirPattern" (isMatchingDirPath finderSettingsOutDirPattern csFindDir @?= False)
         , testCase "isMatchingDirPath hsfind not matching inDirPattern" (isMatchingDirPath finderSettingsOutDirPattern hsFindDir @?= True)
         , testCase "isMatchingDirPath . default settings" (isMatchingDirPath finder "." @?= True)
         , testCase "isMatchingDirPath .. default settings" (isMatchingDirPath finder ".." @?= True)
         , testCase "isMatchingDirPath .git default settings" (isMatchingDirPath finder ".git" @?= False)
         , testCase "isMatchingDirPath .git includeHidden" (isMatchingDirPath finderSettingsIncludeHidden ".git" @?= True)
         ]

getIsMatchingFilePathTests :: IO [Test]
getIsMatchingFilePathTests = do
  let settings = defaultFindSettings
  let finder = getFinder settings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let finderSettingsInExtension = getFinder settingsInExtension
  let settingsOutExtension = settings { outExtensions = [".cs"] }
  let finderSettingsOutExtension = getFinder settingsOutExtension
  let settingsInFilePattern = settings { inFilePatterns = ["Find"] }
  let finderSettingsInFilePattern = getFinder settingsInFilePattern
  let settingsOutFilePattern = settings { outFilePatterns = ["Main"] }
  let finderSettingsOutFilePattern = getFinder settingsOutFilePattern
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderSettingsIncludeHidden = getFinder settingsIncludeHidden
  let finderHsFile = "Finder.hs"
  let finderCsFile = "Finder.cs"
  let mainHsFile = "Main.hs"
  let gitIgnoreFile = ".gitignore"
  return [ testCase "isMatchingFilePath Finder.hs default settings" (isMatchingFilePath finder finderHsFile @?= True)
         , testCase "isMatchingFilePath Finder.hs matching inExtensions" (isMatchingFilePath finderSettingsInExtension finderHsFile @?= True)
         , testCase "isMatchingFilePath Finder.hs not matching inExtensions" (isMatchingFilePath finderSettingsInExtension finderCsFile @?= False)
         , testCase "isMatchingFilePath Finder.hs matching outExtensions" (isMatchingFilePath finderSettingsOutExtension finderCsFile @?= False)
         , testCase "isMatchingFilePath Finder.hs not matching outExtensions" (isMatchingFilePath finderSettingsOutExtension finderHsFile @?= True)
         , testCase "isMatchingFilePath Finder.hs matching inFilePatterns" (isMatchingFilePath finderSettingsInFilePattern finderHsFile @?= True)
         , testCase "isMatchingFilePath Main.hs not matching inFilePatterns" (isMatchingFilePath finderSettingsInFilePattern mainHsFile @?= False)
         , testCase "isMatchingFilePath Main.hs matching outFilePatterns" (isMatchingFilePath finderSettingsOutFilePattern mainHsFile @?= False)
         , testCase "isMatchingFilePath Finder.hs not matching outFilePatterns" (isMatchingFilePath finderSettingsOutFilePattern finderHsFile @?= True)
         , testCase "isMatchingFilePath .gitignore default settings" (isMatchingFilePath finder gitIgnoreFile @?= False)
         , testCase "isMatchingFilePath .gitignore includeHidden" (isMatchingFilePath finderSettingsIncludeHidden gitIgnoreFile @?= True)
         ]

getIsMatchingArchiveFilePathTests :: IO [Test]
getIsMatchingArchiveFilePathTests = do
  let settings = defaultFindSettings
  let finder = getFinder settings
  let settingsInArchiveExtension = settings { inArchiveExtensions = [".zip"] }
  let finderSettingsInArchiveExtension = getFinder settingsInArchiveExtension
  let settingsOutArchiveExtension = settings { outArchiveExtensions = [".gz"] }
  let finderSettingsOutArchiveExtension = getFinder settingsOutArchiveExtension
  let settingsInArchiveFilePattern = settings { inArchiveFilePatterns = ["arch"] }
  let finderSettingsInArchiveFilePattern = getFinder settingsInArchiveFilePattern
  let settingsOutArchiveFilePattern = settings { outArchiveFilePatterns = ["comp"] }
  let finderSettingsOutArchiveFilePattern = getFinder settingsOutArchiveFilePattern
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderSettingsIncludeHidden = getFinder settingsIncludeHidden
  let archiveZipFile = "archive.zip"
  let archiveTarGzFile = "archive.tar.gz"
  let compressedZipFile = "compressed.zip"
  let hiddenArchiveZipFile = ".gitarchive.zip"
  return [ testCase "isMatchingArchiveFilePath archive.zip default settings" (isMatchingArchiveFilePath finder archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath archive.zip matching inArchiveExtensions" (isMatchingArchiveFilePath finderSettingsInArchiveExtension archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath archive.tar.gz not matching inArchiveExtensions" (isMatchingArchiveFilePath finderSettingsInArchiveExtension archiveTarGzFile @?= False)
         , testCase "isMatchingArchiveFilePath archive.tar.gz matching outArchiveExtensions" (isMatchingArchiveFilePath finderSettingsOutArchiveExtension archiveTarGzFile @?= False)
         , testCase "isMatchingArchiveFilePath archive.zip not matching outArchiveExtensions" (isMatchingArchiveFilePath finderSettingsOutArchiveExtension archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath archive.zip matching inArchiveFilePatterns" (isMatchingArchiveFilePath finderSettingsInArchiveFilePattern archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath compressed.zip not matching inArchiveFilePatterns" (isMatchingArchiveFilePath finderSettingsInArchiveFilePattern compressedZipFile @?= False)
         , testCase "isMatchingArchiveFilePath compressed.zip matching outArchiveFilePatterns" (isMatchingArchiveFilePath finderSettingsOutArchiveFilePattern compressedZipFile @?= False)
         , testCase "isMatchingArchiveFilePath archive.zip not matching outArchiveFilePatterns" (isMatchingArchiveFilePath finderSettingsOutArchiveFilePattern archiveZipFile @?= True)
         , testCase "isMatchingArchiveFilePath .gitarchive.zip default settings" (isMatchingArchiveFilePath finder hiddenArchiveZipFile @?= False)
         , testCase "isMatchingArchiveFilePath .gitarchive.zip includeHidden" (isMatchingArchiveFilePath finderSettingsIncludeHidden hiddenArchiveZipFile @?= True)
         ]

getFilterToFileResultTests :: IO [Test]
getFilterToFileResultTests = do
  let settings = defaultFindSettings
  -- jsonFileTypes <- getJsonFileTypes
  let finder = getFinder settings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let finderSettingsInExtension = getFinder settingsInExtension
  let settingsOutExtension = settings { outExtensions = [".hs"] }
  let finderSettingsOutExtension = getFinder settingsOutExtension
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderSettingsIncludeHidden = getFinder settingsIncludeHidden
  let settingsIncludeArchives = settings { includeArchives = True }
  let finderSettingsIncludeArchives = getFinder settingsIncludeArchives
  let settingsArchivesOnly = settingsIncludeArchives { archivesOnly = True }
  let finderSettingsArchivesOnly = getFinder settingsArchivesOnly
  let finderHsFile = ("Finder.hs", Code)
  let gitignoreFile = (".gitignore", Text)
  let archiveZipFile = ("archive.zip", Archive)
  return [ testCase "filterToFileResult Finder.hs default settings" (isJust (filterToFileResult finder finderHsFile) @?= True)
         , testCase "filterToFileResult Finder.hs isMatchingFilePath" (isJust (filterToFileResult finderSettingsInExtension finderHsFile) @?= True)
         , testCase "filterToFileResult Finder.hs not isMatchingFilePath" (isJust (filterToFileResult finderSettingsOutExtension finderHsFile) @?= False)
         , testCase "filterToFileResult .gitignore default settings" (isJust (filterToFileResult finder gitignoreFile) @?= False)
         , testCase "filterToFileResult .gitignore includeHidden" (isJust (filterToFileResult finderSettingsIncludeHidden gitignoreFile) @?= True)
         , testCase "filterToFileResult archive.zip default settings" (isJust (filterToFileResult finder archiveZipFile) @?= False)
         , testCase "filterToFileResult archive.zip includeArchives" (isJust (filterToFileResult finderSettingsIncludeArchives archiveZipFile) @?= True)
         , testCase "filterToFileResult archive.zip archivesOnly" (isJust (filterToFileResult finderSettingsArchivesOnly archiveZipFile) @?= True)
         , testCase "filterToFileResult Finder.hs archivesOnly" (isJust (filterToFileResult finderSettingsArchivesOnly finderHsFile) @?= False)
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
  let finder = getFinder settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFindPythonFileResultTests" (True @?= False)]
    Right fileResults ->
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
  let finder = getFinder settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFindRubyFileResultTests" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFindRubyFileResultTests" (length fileResults @?= 0)
               ]

getFollowSymlinksDefaultTests :: IO [Test]
getFollowSymlinksDefaultTests = do
  xfindPath <- getXfindPath
  let settings = defaultFindSettings {
    paths = [xfindPath ++ "/bin"]
  }
  let finder = getFinder settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFollowSymlinksTests defaultSettings" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFollowSymlinksTests defaultSettings" ((length fileResults < 4) @?= True)
               ]

getFollowSymlinksTrueTests :: IO [Test]
getFollowSymlinksTrueTests = do
  xfindPath <- getXfindPath
  let settings = defaultFindSettings {
    paths = [xfindPath ++ "/bin"],
    followSymlinks = True
  }
  let finder = getFinder settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFollowSymlinksTests followSymlinks" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFollowSymlinksTests followSymlinks" ((length fileResults > 2) @?= True)
               ]

getFollowSymlinksFalseTests :: IO [Test]
getFollowSymlinksFalseTests = do
  xfindPath <- getXfindPath
  let settings = defaultFindSettings {
    paths = [xfindPath ++ "/bin"],
    followSymlinks = False
  }
  let finder = getFinder settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFollowSymlinksTests noFollowSymlinks" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFollowSymlinksTests noFollowSymlinks" ((length fileResults < 4) @?= True)
               ]
