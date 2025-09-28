module HsFind.FilterTests
    ( FilterTests(..)
    , getFilterTests
    ) where

import Data.Maybe (fromJust, isNothing)

import System.FilePath (takeFileName)
import Text.Regex.PCRE ( (=~) )
import Data.Time (UTCTime)

import HsFind.FileTypes (FileType(..))
import HsFind.FileUtil (getParentPath, hasExtension, isHiddenFilePath)
import HsFind.FileResult (FileResult(..))
import HsFind.FindSettings (FindSettings(..))


data FilterTests = FilterTests
  { pathByHiddenTests :: [FilePath -> Bool]
  , dirPathByInPatternsTests :: [FilePath -> Bool]
  , dirPathByOutPatternsTests :: [FilePath -> Bool]
  , dirPathTests :: [FilePath -> Bool]
  , filePathByInExtensionsTests :: [FilePath -> Bool]
  , filePathByOutExtensionsTests :: [FilePath -> Bool]
  , filePathByInPatternsTests :: [FilePath -> Bool]
  , filePathByOutPatternsTests :: [FilePath -> Bool]
  , filePathTests :: [FilePath -> Bool]
  , fileTypeByInFileTypesTests :: [FileType -> Bool]
  , fileTypeByOutFileTypesTests :: [FileType -> Bool]
  , fileTypeTests :: [FileType -> Bool]
  , fileSizeByMaxSizeTests :: [Integer -> Bool]
  , fileSizeByMinSizeTests :: [Integer -> Bool]
  , fileSizeTests :: [Integer -> Bool]
  , lastModByMaxLastModTests :: [Maybe UTCTime -> Bool]
  , lastModByMinLastModTests :: [Maybe UTCTime -> Bool]
  , lastModTests :: [Maybe UTCTime -> Bool]
  , fileResultTests :: [FileResult -> Bool]
  , archiveFilePathByInExtensionsTests :: [FilePath -> Bool]
  , archiveFilePathByOutExtensionsTests :: [FilePath -> Bool]
  , archiveFilePathByInPatternsTests :: [FilePath -> Bool]
  , archiveFilePathByOutPatternsTests :: [FilePath -> Bool]
  , archiveFilePathTests :: [FilePath -> Bool]
  , archiveFileResultTests :: [FileResult -> Bool]
  }

getPathByHiddenTests :: FindSettings -> [FilePath -> Bool]
getPathByHiddenTests settings =
  hiddenPathTests
  where hiddenPathTests | includeHidden settings = []
                        | otherwise = [not . isHiddenFilePath]

getDirPathByInPatternsTests :: FindSettings -> [FilePath -> Bool]
getDirPathByInPatternsTests settings = inPatternTests
  where inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> fp =~ p :: Bool) inPatterns]
        inPatterns = inDirPatterns settings

getDirPathByOutPatternsTests :: FindSettings -> [FilePath -> Bool]
getDirPathByOutPatternsTests settings = outPatternTests
  where outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ fp =~ p :: Bool) outPatterns]
        outPatterns = outDirPatterns settings

getArchiveFilePathByInExtensionsTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathByInExtensionsTests settings = inExtTests
  where inExtTests      | null inExts = []
                        | otherwise = [\fp -> any (hasExtension fp) inExts]
        inExts = inArchiveExtensions settings

getArchiveFilePathByOutExtensionsTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathByOutExtensionsTests settings = outExtTests
  where outExtTests     | null outExts = []
                        | otherwise = [\fp -> not $ any (hasExtension fp) outExts]
        outExts = outArchiveExtensions settings

getArchiveFilePathByInPatternsTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathByInPatternsTests settings = inPatternTests
  where inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> takeFileName fp =~ p :: Bool) inPatterns]
        inPatterns = inArchiveFilePatterns settings

getArchiveFilePathByOutPatternsTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathByOutPatternsTests settings = outPatternTests
  where outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ takeFileName fp =~ p :: Bool) outPatterns]
        outPatterns = outArchiveFilePatterns settings

getFilePathByInExtensionsTests :: FindSettings -> [FilePath -> Bool]
getFilePathByInExtensionsTests settings = inExtTests
  where inExtTests      | null inExts = []
                        | otherwise = [\fp -> any (hasExtension fp) inExts]
        inExts = inExtensions settings

getFilePathByOutExtensionsTests :: FindSettings -> [FilePath -> Bool]
getFilePathByOutExtensionsTests settings = outExtTests
  where outExtTests     | null outExts = []
                        | otherwise = [\fp -> not $ any (hasExtension fp) outExts]
        outExts = outExtensions settings

getFilePathByInPatternsTests :: FindSettings -> [FilePath -> Bool]
getFilePathByInPatternsTests settings = inPatternTests
  where inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> takeFileName fp =~ p :: Bool) inPatterns]
        inPatterns = inFilePatterns settings

getFilePathByOutPatternsTests :: FindSettings -> [FilePath -> Bool]
getFilePathByOutPatternsTests settings = outPatternTests
  where outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ takeFileName fp =~ p :: Bool) outPatterns]
        outPatterns = outFilePatterns settings

getFileTypeByInFileTypesTests :: FindSettings -> [FileType -> Bool]
getFileTypeByInFileTypesTests settings = inFileTypeTests
  where inFileTypeTests  | null inTypes = []
                         | otherwise = [(`elem` inTypes)]
        inTypes = inFileTypes settings

getFileTypeByOutFileTypesTests :: FindSettings -> [FileType -> Bool]
getFileTypeByOutFileTypesTests settings = outFileTypeTests
  where outFileTypeTests | null outTypes = []
                         | otherwise = [(`notElem` outTypes)]
        outTypes = outFileTypes settings

getFileSizeByMaxSizeTests :: FindSettings -> [Integer -> Bool]
getFileSizeByMaxSizeTests settings = maxSizeTests
  where maxSizeTests | maxSize settings == 0 = []
                     | otherwise = [\i -> i <= maxSize settings]

getFileSizeByMinSizeTests :: FindSettings -> [Integer -> Bool]
getFileSizeByMinSizeTests settings = minSizeTests
  where minSizeTests | minSize settings == 0 = []
                     | otherwise = [\i -> i >= minSize settings]

getLastModByMaxLastModTests :: FindSettings -> [Maybe UTCTime -> Bool]
getLastModByMaxLastModTests settings = maxLastModTests
  where maxLastModTests | isNothing (maxLastMod settings) = []
                        | otherwise = [\lastMod -> fromJust lastMod <= fromJust (maxLastMod settings)]

getLastModByMinLastModTests :: FindSettings -> [Maybe UTCTime -> Bool]
getLastModByMinLastModTests settings = minLastModTests
  where minLastModTests | isNothing (minLastMod settings) = []
                        | otherwise = [\lastMod -> fromJust lastMod >= fromJust (minLastMod settings)]

getFilterTests :: FindSettings -> FilterTests
getFilterTests settings = FilterTests
  { pathByHiddenTests = pathByHiddenTests
  , dirPathByInPatternsTests = dirPathByInPatternsTests
  , dirPathByOutPatternsTests = dirPathByOutPatternsTests
  , dirPathTests = dirPathTests
  , filePathByInExtensionsTests = filePathByInExtensionsTests
  , filePathByOutExtensionsTests = filePathByOutExtensionsTests
  , filePathByInPatternsTests = filePathByInPatternsTests
  , filePathByOutPatternsTests = filePathByOutPatternsTests
  , filePathTests = filePathTests
  , fileTypeByInFileTypesTests = fileTypeByInFileTypesTests
  , fileTypeByOutFileTypesTests = fileTypeByOutFileTypesTests
  , fileTypeTests = fileTypeTests
  , fileSizeByMaxSizeTests = fileSizeByMaxSizeTests
  , fileSizeByMinSizeTests = fileSizeByMinSizeTests
  , fileSizeTests = fileSizeTests
  , lastModByMaxLastModTests = lastModByMaxLastModTests
  , lastModByMinLastModTests = lastModByMinLastModTests
  , lastModTests = lastModTests
  , fileResultTests = fileResultTests
  , archiveFilePathByInExtensionsTests = archiveFilePathByInExtensionsTests
  , archiveFilePathByOutExtensionsTests = archiveFilePathByOutExtensionsTests
  , archiveFilePathByInPatternsTests = archiveFilePathByInPatternsTests
  , archiveFilePathByOutPatternsTests = archiveFilePathByOutPatternsTests
  , archiveFilePathTests = archiveFilePathTests
  , archiveFileResultTests = archiveFileResultTests
  }
  where
    pathByHiddenTests = getPathByHiddenTests settings
    dirPathByInPatternsTests = getDirPathByInPatternsTests settings
    dirPathByOutPatternsTests = getDirPathByOutPatternsTests settings
    dirPathTests = pathByHiddenTests ++ dirPathByInPatternsTests ++ dirPathByOutPatternsTests
    filePathByInExtensionsTests = getFilePathByInExtensionsTests settings
    filePathByOutExtensionsTests = getFilePathByOutExtensionsTests settings
    filePathByInPatternsTests = getFilePathByInPatternsTests settings
    filePathByOutPatternsTests = getFilePathByOutPatternsTests settings
    filePathTests = pathByHiddenTests ++ filePathByInExtensionsTests ++ filePathByOutExtensionsTests ++ filePathByInPatternsTests ++ filePathByOutPatternsTests
    fileTypeByInFileTypesTests = getFileTypeByInFileTypesTests settings
    fileTypeByOutFileTypesTests = getFileTypeByOutFileTypesTests settings
    fileTypeTests = fileTypeByInFileTypesTests ++ fileTypeByOutFileTypesTests
    fileSizeByMaxSizeTests = getFileSizeByMaxSizeTests settings
    fileSizeByMinSizeTests = getFileSizeByMinSizeTests settings
    fileSizeTests = fileSizeByMaxSizeTests ++ fileSizeByMinSizeTests
    lastModByMaxLastModTests = getLastModByMaxLastModTests settings
    lastModByMinLastModTests = getLastModByMinLastModTests settings
    lastModTests = lastModByMaxLastModTests ++ lastModByMinLastModTests
    fileResultDirTests = map (. (getParentPath . fileResultPath)) dirPathTests
    fileResultPathTests = map (. fileResultPath) filePathTests
    fileResultTypeTests = map (. fileResultType) fileTypeTests
    fileResultSizeTests = map (. fileResultSize) fileSizeTests
    fileResultLastModTests = map (. fileLastMod) lastModTests
    fileResultTests = fileResultDirTests ++ fileResultPathTests ++ fileResultTypeTests ++ fileResultSizeTests ++ fileResultLastModTests
    archiveFilePathByInExtensionsTests = getArchiveFilePathByInExtensionsTests settings
    archiveFilePathByOutExtensionsTests = getArchiveFilePathByOutExtensionsTests settings
    archiveFilePathByInPatternsTests = getArchiveFilePathByInPatternsTests settings
    archiveFilePathByOutPatternsTests = getArchiveFilePathByOutPatternsTests settings
    archiveFilePathTests = pathByHiddenTests ++ archiveFilePathByInExtensionsTests ++ archiveFilePathByOutExtensionsTests ++ archiveFilePathByInPatternsTests ++ archiveFilePathByOutPatternsTests
    archiveFileResultTests = map (. fileResultPath) archiveFilePathTests
