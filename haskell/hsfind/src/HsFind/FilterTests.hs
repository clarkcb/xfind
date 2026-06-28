module HsFind.FilterTests
    ( FilterTests(..)
    , getFilterTests
    ) where

import Data.Maybe (fromJust, isJust, isNothing)

import System.FilePath (takeFileName, splitDirectories)
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

anyMatchesAnyPattern :: [String] -> [String] -> Bool
anyMatchesAnyPattern strings patterns = any (\s -> any (\p -> s =~ p :: Bool) patterns) strings

getFilterDirPathByInPatterns :: [String] -> (FilePath -> Bool)
getFilterDirPathByInPatterns inPatterns = doFilter
  where doFilter | null inPatterns = const True
                 | otherwise = \p -> anyMatchesAnyPattern (splitDirectories p) inPatterns

getFilterDirPathByOutPatterns :: [String] -> (FilePath -> Bool)
getFilterDirPathByOutPatterns outPatterns = doFilter
  where doFilter | null outPatterns = const True
                 | otherwise = \p -> not $ anyMatchesAnyPattern (splitDirectories p) outPatterns

getFilterFilePathByHidden :: Bool -> (FilePath -> Bool)
getFilterFilePathByHidden includeHidden' = doFilter
  where doFilter | includeHidden' = const True
                 | otherwise = not . isHiddenFilePath

getFilterFilePathByInExtensions :: [String] -> (FilePath -> Bool)
getFilterFilePathByInExtensions inExtensions' = doFilter
  where doFilter | null inExtensions' = const True
                 | otherwise = \fp -> any (hasExtension fp) inExtensions'

getFilterFilePathByOutExtensions :: [String] -> (FilePath -> Bool)
getFilterFilePathByOutExtensions outExtensions' = doFilter
  where doFilter | null outExtensions' = const True
                 | otherwise = \fp -> not (any (hasExtension fp) outExtensions')

getFilterFilePathByInPatterns :: [String] -> (FilePath -> Bool)
getFilterFilePathByInPatterns inPatterns = doFilter
  where doFilter | null inPatterns = const True
                 | otherwise = \fp -> any (\p -> takeFileName fp =~ p :: Bool) inPatterns

getFilterFilePathByOutPatterns :: [String] -> (FilePath -> Bool)
getFilterFilePathByOutPatterns outPatterns = doFilter
  where doFilter | null outPatterns = const True
                 | otherwise = \fp -> all (\p -> not $ takeFileName fp =~ p :: Bool) outPatterns

getFilterFileTypeByInFileTypes :: [FileType] -> (FileType -> Bool)
getFilterFileTypeByInFileTypes inFileTypes' = doFilter
  where doFilter | null inFileTypes' = const True
                 | otherwise = (`elem` inFileTypes')

getFilterFileTypeByOutFileTypes :: [FileType] -> (FileType -> Bool)
getFilterFileTypeByOutFileTypes outFileTypes' = doFilter
  where doFilter | null outFileTypes' = const True
                 | otherwise = (`notElem` outFileTypes')

getFilterFileSizeByMaxSize :: Integer -> (Integer -> Bool)
getFilterFileSizeByMaxSize maxSize' = doFilter
  where doFilter | maxSize' == 0 = const True
                 | otherwise = (<= maxSize')

getFilterFileSizeByMinSize :: Integer -> (Integer -> Bool)
getFilterFileSizeByMinSize minSize' = doFilter
  where doFilter | minSize' == 0 = const True
                 | otherwise = (>= minSize')

getFilterLastModByMaxLastMod :: Maybe UTCTime -> (Maybe UTCTime -> Bool)
getFilterLastModByMaxLastMod maybeMaxLastMod = doFilter
  where doFilter | isNothing maybeMaxLastMod = const True
                 | otherwise = \mlm -> isJust mlm && (fromJust mlm <= fromJust maybeMaxLastMod)

getFilterLastModByMinLastMod :: Maybe UTCTime -> (Maybe UTCTime -> Bool)
getFilterLastModByMinLastMod maybeMinLastMod = doFilter
  where doFilter | isNothing maybeMinLastMod = const True
                 | otherwise = \mlm -> isJust mlm && (fromJust mlm >= fromJust maybeMinLastMod)

getPathByHiddenTests :: FindSettings -> [FilePath -> Bool]
getPathByHiddenTests settings = [getFilterFilePathByHidden (includeHidden settings)]

getDirPathByInPatternsTests :: FindSettings -> [FilePath -> Bool]
getDirPathByInPatternsTests settings = [getFilterDirPathByInPatterns (inDirPatterns settings)]

getDirPathByOutPatternsTests :: FindSettings -> [FilePath -> Bool]
getDirPathByOutPatternsTests settings = [getFilterDirPathByOutPatterns (outDirPatterns settings)]

getArchiveFilePathByInExtensionsTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathByInExtensionsTests settings = [getFilterFilePathByInExtensions (inArchiveExtensions settings)]

getArchiveFilePathByOutExtensionsTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathByOutExtensionsTests settings = [getFilterFilePathByOutExtensions (outArchiveExtensions settings)]

getArchiveFilePathByInPatternsTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathByInPatternsTests settings = [getFilterFilePathByInPatterns (inArchiveFilePatterns settings)]

getArchiveFilePathByOutPatternsTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathByOutPatternsTests settings = [getFilterFilePathByOutPatterns (outArchiveFilePatterns settings)]

getFilePathByInExtensionsTests :: FindSettings -> [FilePath -> Bool]
getFilePathByInExtensionsTests settings = [getFilterFilePathByInExtensions (inExtensions settings)]

getFilePathByOutExtensionsTests :: FindSettings -> [FilePath -> Bool]
getFilePathByOutExtensionsTests settings = [getFilterFilePathByOutExtensions (outExtensions settings)]

getFilePathByInPatternsTests :: FindSettings -> [FilePath -> Bool]
getFilePathByInPatternsTests settings = [getFilterFilePathByInPatterns (inFilePatterns settings)]

getFilePathByOutPatternsTests :: FindSettings -> [FilePath -> Bool]
getFilePathByOutPatternsTests settings = [getFilterFilePathByOutPatterns (outFilePatterns settings)]

getFileTypeByInFileTypesTests :: FindSettings -> [FileType -> Bool]
getFileTypeByInFileTypesTests settings = [getFilterFileTypeByInFileTypes (inFileTypes settings)]

getFileTypeByOutFileTypesTests :: FindSettings -> [FileType -> Bool]
getFileTypeByOutFileTypesTests settings = [getFilterFileTypeByOutFileTypes (outFileTypes settings)]

getFileSizeByMaxSizeTests :: FindSettings -> [Integer -> Bool]
getFileSizeByMaxSizeTests settings = [getFilterFileSizeByMaxSize (maxSize settings)]

getFileSizeByMinSizeTests :: FindSettings -> [Integer -> Bool]
getFileSizeByMinSizeTests settings = [getFilterFileSizeByMinSize (minSize settings)]

getLastModByMaxLastModTests :: FindSettings -> [Maybe UTCTime -> Bool]
getLastModByMaxLastModTests settings = [getFilterLastModByMaxLastMod (maxLastMod settings)]

getLastModByMinLastModTests :: FindSettings -> [Maybe UTCTime -> Bool]
getLastModByMinLastModTests settings = [getFilterLastModByMinLastMod (minLastMod settings)]

getFilterTests :: FindSettings -> FilterTests
getFilterTests settings = FilterTests
  { pathByHiddenTests = pathByHiddenTests'
  , dirPathByInPatternsTests = dirPathByInPatternsTests'
  , dirPathByOutPatternsTests = dirPathByOutPatternsTests'
  , dirPathTests = dirPathTests'
  , filePathByInExtensionsTests = filePathByInExtensionsTests'
  , filePathByOutExtensionsTests = filePathByOutExtensionsTests'
  , filePathByInPatternsTests = filePathByInPatternsTests'
  , filePathByOutPatternsTests = filePathByOutPatternsTests'
  , filePathTests = filePathTests'
  , fileTypeByInFileTypesTests = fileTypeByInFileTypesTests'
  , fileTypeByOutFileTypesTests = fileTypeByOutFileTypesTests'
  , fileTypeTests = fileTypeTests'
  , fileSizeByMaxSizeTests = fileSizeByMaxSizeTests'
  , fileSizeByMinSizeTests = fileSizeByMinSizeTests'
  , fileSizeTests = fileSizeTests'
  , lastModByMaxLastModTests = lastModByMaxLastModTests'
  , lastModByMinLastModTests = lastModByMinLastModTests'
  , lastModTests = lastModTests'
  , fileResultTests = fileResultTests'
  , archiveFilePathByInExtensionsTests = archiveFilePathByInExtensionsTests'
  , archiveFilePathByOutExtensionsTests = archiveFilePathByOutExtensionsTests'
  , archiveFilePathByInPatternsTests = archiveFilePathByInPatternsTests'
  , archiveFilePathByOutPatternsTests = archiveFilePathByOutPatternsTests'
  , archiveFilePathTests = archiveFilePathTests'
  , archiveFileResultTests = archiveFileResultTests'
  }
  where
    pathByHiddenTests' = getPathByHiddenTests settings
    dirPathByInPatternsTests' = getDirPathByInPatternsTests settings
    dirPathByOutPatternsTests' = getDirPathByOutPatternsTests settings
    dirPathTests' = pathByHiddenTests' ++ dirPathByInPatternsTests' ++ dirPathByOutPatternsTests'
    filePathByInExtensionsTests' = getFilePathByInExtensionsTests settings
    filePathByOutExtensionsTests' = getFilePathByOutExtensionsTests settings
    filePathByInPatternsTests' = getFilePathByInPatternsTests settings
    filePathByOutPatternsTests' = getFilePathByOutPatternsTests settings
    filePathTests' = pathByHiddenTests' ++ filePathByInExtensionsTests' ++ filePathByOutExtensionsTests' ++ filePathByInPatternsTests' ++ filePathByOutPatternsTests'
    fileTypeByInFileTypesTests' = getFileTypeByInFileTypesTests settings
    fileTypeByOutFileTypesTests' = getFileTypeByOutFileTypesTests settings
    fileTypeTests' = fileTypeByInFileTypesTests' ++ fileTypeByOutFileTypesTests'
    fileSizeByMaxSizeTests' = getFileSizeByMaxSizeTests settings
    fileSizeByMinSizeTests' = getFileSizeByMinSizeTests settings
    fileSizeTests' = fileSizeByMaxSizeTests' ++ fileSizeByMinSizeTests'
    lastModByMaxLastModTests' = getLastModByMaxLastModTests settings
    lastModByMinLastModTests' = getLastModByMinLastModTests settings
    lastModTests' = lastModByMaxLastModTests' ++ lastModByMinLastModTests'
    fileResultDirTests' = map (. (getParentPath . fileResultPath)) dirPathTests'
    fileResultPathTests' = map (. fileResultPath) filePathTests'
    fileResultTypeTests' = map (. fileResultType) fileTypeTests'
    fileResultSizeTests' = map (. fileResultSize) fileSizeTests'
    fileResultLastModTests' = map (. fileLastMod) lastModTests'
    fileResultTests' = fileResultDirTests' ++ fileResultPathTests' ++ fileResultTypeTests' ++ fileResultSizeTests' ++ fileResultLastModTests'
    archiveFilePathByInExtensionsTests' = getArchiveFilePathByInExtensionsTests settings
    archiveFilePathByOutExtensionsTests' = getArchiveFilePathByOutExtensionsTests settings
    archiveFilePathByInPatternsTests' = getArchiveFilePathByInPatternsTests settings
    archiveFilePathByOutPatternsTests' = getArchiveFilePathByOutPatternsTests settings
    archiveFilePathTests' = pathByHiddenTests' ++ archiveFilePathByInExtensionsTests' ++ archiveFilePathByOutExtensionsTests' ++ archiveFilePathByInPatternsTests' ++ archiveFilePathByOutPatternsTests'
    archiveFileResultTests' = map (. fileResultPath) archiveFilePathTests'
