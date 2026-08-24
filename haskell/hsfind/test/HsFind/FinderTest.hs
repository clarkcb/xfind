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

import HsFind.FileTypes
import HsFind.FindConfig
import HsFind.FindSettings
import HsFind.Finder

import Data.Maybe (isJust)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)


getIsMatchingDirPathTests :: IO [Test]
getIsMatchingDirPathTests = do
  config <- getFindConfig
  let settings = defaultFindSettings
  let finder = getFinder config settings
  let settingsInDirPattern = settings { inDirPatterns = ["hs"] }
  let finderSettingsInDirPattern = getFinder config settingsInDirPattern
  let settingsOutDirPattern = settings { outDirPatterns = ["cs"] }
  let finderSettingsOutDirPattern = getFinder config settingsOutDirPattern
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderSettingsIncludeHidden = getFinder config settingsIncludeHidden
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
  config <- getFindConfig
  let settings = defaultFindSettings
  let finder = getFinder config settings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let finderSettingsInExtension = getFinder config settingsInExtension
  let settingsOutExtension = settings { outExtensions = [".cs"] }
  let finderSettingsOutExtension = getFinder config settingsOutExtension
  let settingsInFilePattern = settings { inFilePatterns = ["Find"] }
  let finderSettingsInFilePattern = getFinder config settingsInFilePattern
  let settingsOutFilePattern = settings { outFilePatterns = ["Main"] }
  let finderSettingsOutFilePattern = getFinder config settingsOutFilePattern
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderSettingsIncludeHidden = getFinder config settingsIncludeHidden
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
  config <- getFindConfig
  let settings = defaultFindSettings
  let finder = getFinder config settings
  let settingsInArchiveExtension = settings { inArchiveExtensions = [".zip"] }
  let finderSettingsInArchiveExtension = getFinder config settingsInArchiveExtension
  let settingsOutArchiveExtension = settings { outArchiveExtensions = [".gz"] }
  let finderSettingsOutArchiveExtension = getFinder config settingsOutArchiveExtension
  let settingsInArchiveFilePattern = settings { inArchiveFilePatterns = ["arch"] }
  let finderSettingsInArchiveFilePattern = getFinder config settingsInArchiveFilePattern
  let settingsOutArchiveFilePattern = settings { outArchiveFilePatterns = ["comp"] }
  let finderSettingsOutArchiveFilePattern = getFinder config settingsOutArchiveFilePattern
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderSettingsIncludeHidden = getFinder config settingsIncludeHidden
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
  config <- getFindConfig
  let settings = defaultFindSettings
  let finder = getFinder config settings
  let settingsInExtension = settings { inExtensions = [".hs"] }
  let finderSettingsInExtension = getFinder config settingsInExtension
  let settingsOutExtension = settings { outExtensions = [".hs"] }
  let finderSettingsOutExtension = getFinder config settingsOutExtension
  let settingsIncludeHidden = settings { includeHidden = True }
  let finderSettingsIncludeHidden = getFinder config settingsIncludeHidden
  let settingsIncludeArchives = settings { includeArchives = True }
  let finderSettingsIncludeArchives = getFinder config settingsIncludeArchives
  let settingsArchivesOnly = settingsIncludeArchives { archivesOnly = True }
  let finderSettingsArchivesOnly = getFinder config settingsArchivesOnly
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
  config <- getFindConfig
  let settings = defaultFindSettings {
    debug = True,
    inFileTypes = [Audio],
    outFilePatterns = ["build", "cmake", "node_modules", "pycache", "vendor", "venv"],
    paths = ["/Users/cary/src/xfind/python"]
  }
  let finder = getFinder config settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFindPythonFileResultTests" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFindPythonFileResultTests" (length fileResults @?= 0)
               ]

-- hsfind -D build -D cmake -D node_modules -D vendor -D venv -t audio /Users/cary/src/xfind/ruby --debug
getFindRubyFileResultTests :: IO [Test]
getFindRubyFileResultTests = do
  config <- getFindConfig
  let settings = defaultFindSettings {
    debug = True,
    inFileTypes = [Audio],
    outFilePatterns = ["build", "cmake", "node_modules", "pycache", "vendor", "venv"],
    paths = ["/Users/cary/src/xfind/ruby"]
  }
  let finder = getFinder config settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFindRubyFileResultTests" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFindRubyFileResultTests" (length fileResults @?= 0)
               ]

getFollowSymlinksDefaultTests :: IO [Test]
getFollowSymlinksDefaultTests = do
  config <- getFindConfig
  let settings = defaultFindSettings {
    paths = [xfindPath config ++ "/bin"]
  }
  let finder = getFinder config settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFollowSymlinksTests defaultSettings" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFollowSymlinksTests defaultSettings" ((length fileResults < 4) @?= True)
               ]

getFollowSymlinksTrueTests :: IO [Test]
getFollowSymlinksTrueTests = do
  config <- getFindConfig
  let settings = defaultFindSettings {
    paths = [xfindPath config ++ "/bin"],
    followSymlinks = True
  }
  let finder = getFinder config settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFollowSymlinksTests followSymlinks" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFollowSymlinksTests followSymlinks" ((length fileResults > 2) @?= True)
               ]

getFollowSymlinksFalseTests :: IO [Test]
getFollowSymlinksFalseTests = do
  config <- getFindConfig
  let settings = defaultFindSettings {
    paths = [xfindPath config ++ "/bin"],
    followSymlinks = False
  }
  let finder = getFinder config settings
  fileResultsEither <- doFind finder
  case fileResultsEither of
    Left _ -> return [ testCase "getFollowSymlinksTests noFollowSymlinks" (True @?= False)]
    Right fileResults ->
        return [ testCase "getFollowSymlinksTests noFollowSymlinks" ((length fileResults < 4) @?= True)
               ]
