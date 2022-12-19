module HsFind.Finder
    (
      doFind
    , filterFile
    , filterToFileResult
    , getFileResults
    , isMatchingArchiveFile
    , isMatchingDir
    , isMatchingFile
    ) where

import Control.Monad (forM)
import Data.List (sortBy)
import Data.Maybe (fromJust, isJust)
import qualified Data.ByteString as B
import System.FilePath (dropFileName, takeFileName)
import Text.Regex.PCRE ( (=~) )

import HsFind.FileTypes (FileType, JsonFileType, getFileTypes, getJsonFileTypes, fileTypeFromJsonFileTypes)
import HsFind.FileUtil
    (hasExtension, isHiddenFilePath, getRecursiveFilteredContents)
import HsFind.FileResult
    (FileResult(..), blankFileResult, isArchiveFile, newFileResult)
import HsFind.FindSettings
import GHC.Generics (Generic1(from1))


getDirTests :: FindSettings -> [FilePath -> Bool]
getDirTests settings = hiddenPathTests ++ inPatternTests ++ outPatternTests
  where hiddenPathTests = [\fp -> includeHidden || not (isHiddenFilePath fp)]
        includeHidden = not $ excludeHidden settings
        inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> fp =~ p :: Bool) inPatterns]
        outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ fp =~ p :: Bool) outPatterns]
        inPatterns = inDirPatterns settings
        outPatterns = outDirPatterns settings

matchesDirTests :: [FilePath -> Bool] -> FilePath -> Bool
matchesDirTests tests d = all ($d) tests

isMatchingDir :: FindSettings -> FilePath -> Bool
isMatchingDir settings = matchesDirTests dirTests
  where dirTests :: [FilePath -> Bool]
        dirTests = getDirTests settings

getFileTests :: FindSettings -> [FilePath -> Bool]
getFileTests settings =
  hiddenPathTests ++ inExtTests ++ outExtTests ++ inPatternTests ++ outPatternTests
  where hiddenPathTests = [\fp -> includeHidden || not (isHiddenFilePath fp)]
        includeHidden = not $ excludeHidden settings
        inExtTests      | null inExts = []
                        | otherwise = [\fp -> any (hasExtension fp) inExts]
        outExtTests     | null outExts = []
                        | otherwise = [\fp -> not $ any (hasExtension fp) outExts]
        inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> fp =~ p :: Bool) inPatterns]
        outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ fp =~ p :: Bool) outPatterns]
        inExts = inExtensions settings
        outExts = outExtensions settings
        inPatterns = inFilePatterns settings
        outPatterns = outFilePatterns settings

getAllFileTests :: FindSettings -> [JsonFileType] -> [FilePath -> Bool]
getAllFileTests settings jsonFileTypes =
  hiddenPathTests ++ inExtTests ++ outExtTests ++ inPatternTests ++ outPatternTests ++
    inFileTypeTests ++ outFileTypeTests
  where hiddenPathTests = [\fp -> includeHidden || not (isHiddenFilePath fp)]
        includeHidden = not $ excludeHidden settings
        inExtTests       | null inExts = []
                         | otherwise = [\fp -> any (hasExtension fp) inExts]
        outExtTests      | null outExts = []
                         | otherwise = [\fp -> not $ any (hasExtension fp) outExts]
        inPatternTests   | null inPatterns = []
                         | otherwise = [\fp -> any (\p -> fp =~ p :: Bool) inPatterns]
        outPatternTests  | null outPatterns = []
                         | otherwise = [\fp -> all (\p -> not $ fp =~ p :: Bool) outPatterns]
        inFileTypeTests  | null inTypes = []
                         | otherwise = [\fp -> getFileType fp `elem` inTypes]
        outFileTypeTests | null outTypes = []
                         | otherwise = [\fp -> getFileType fp `notElem` outTypes]
        inExts = inExtensions settings
        outExts = outExtensions settings
        inPatterns = inFilePatterns settings
        outPatterns = outFilePatterns settings
        inTypes = inFileTypes settings
        outTypes = outFileTypes settings
        getFileType = fileTypeFromJsonFileTypes jsonFileTypes

matchesFileTests :: [FilePath -> Bool] -> FilePath -> Bool
matchesFileTests tests f = all ($f) tests

isMatchingFile :: FindSettings -> FilePath -> Bool
isMatchingFile settings = matchesFileTests fileTests
  where fileTests :: [FilePath -> Bool]
        fileTests = getFileTests settings

getArchiveFileTests :: FindSettings -> [FilePath -> Bool]
getArchiveFileTests settings =
  hiddenPathTests ++ inExtTests ++ outExtTests ++ inPatternTests ++ outPatternTests
  where hiddenPathTests = [\fp -> includeHidden || not (isHiddenFilePath fp)]
        includeHidden = not $ excludeHidden settings
        inExtTests      | null inExts = []
                        | otherwise = [\fp -> any (hasExtension fp) inExts]
        outExtTests     | null outExts = []
                        | otherwise = [\fp -> not $ any (hasExtension fp) outExts]
        inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> fp =~ p :: Bool) inPatterns]
        outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ fp =~ p :: Bool) outPatterns]
        inExts = inArchiveExtensions settings
        outExts = outArchiveExtensions settings
        inPatterns = inArchiveFilePatterns settings
        outPatterns = outArchiveFilePatterns settings

matchesArchiveFileTests :: [FilePath -> Bool] -> FilePath -> Bool
matchesArchiveFileTests tests f = all ($f) tests

isMatchingArchiveFile :: FindSettings -> FilePath -> Bool
isMatchingArchiveFile settings = matchesArchiveFileTests archiveFileTests
  where archiveFileTests :: [FilePath -> Bool]
        archiveFileTests = getArchiveFileTests settings

filterFile :: FindSettings -> FileResult -> Bool
filterFile settings fr | isArchiveFile fr = includeArchiveFile fr
                       | otherwise        = includeFile fr
  where includeArchiveFile f = includeArchives settings &&
                               isMatchingArchiveFile settings (fileResultPath f)
        includeFile f = not (archivesOnly settings) &&
                        isMatchingFile settings (fileResultPath f)

filterToFileResult :: FindSettings -> [JsonFileType] -> (FilePath,FileType) -> Maybe FileResult
filterToFileResult settings jsonFileTypes ft =
  if (null inTypes || snd ft `elem` inTypes) && (null outTypes || notElem (snd ft) outTypes)
  then Just $ uncurry newFileResult ft
  else Nothing
  where inTypes = inFileTypes settings
        outTypes = outFileTypes settings

getFileResult :: (FilePath,FileType) -> FileResult
getFileResult = uncurry newFileResult

getFileResults :: FindSettings -> IO [FileResult]
getFileResults settings = do
  let dirTests = getDirTests settings
  jsonFileTypes <- getJsonFileTypes
  let fileTests = getAllFileTests settings jsonFileTypes
  paths <- forM (paths settings) $ \path ->
    getRecursiveFilteredContents path (matchesDirTests dirTests) (matchesFileTests fileTests)
  let allPaths = concat paths
  allFileTypes <- getFileTypes allPaths
  return $ zipWith (curry getFileResult) allPaths allFileTypes

sortFileResultsByPath :: FileResult -> FileResult -> Ordering
sortFileResultsByPath fr1 fr2 =
  if p1 == p2
  then compare f1 f2
  else compare p1 p2
  where p1 = dropFileName (fileResultPath fr1)
        p2 = dropFileName (fileResultPath fr2)
        f1 = takeFileName (fileResultPath fr1)
        f2 = takeFileName (fileResultPath fr2)

sortFileResultsByName :: FileResult -> FileResult -> Ordering
sortFileResultsByName fr1 fr2 =
  if f1 == f2
  then compare p1 p2
  else compare f1 f2
  where p1 = dropFileName (fileResultPath fr1)
        p2 = dropFileName (fileResultPath fr2)
        f1 = takeFileName (fileResultPath fr1)
        f2 = takeFileName (fileResultPath fr2)

sortFileResultsByType :: FileResult -> FileResult -> Ordering
sortFileResultsByType fr1 fr2 =
  if t1 == t2
  then sortFileResultsByPath fr1 fr2
  else compare t1 t2
  where t1 = fileResultType fr1
        t2 = fileResultType fr2

sortFileResults :: FindSettings -> [FileResult] -> [FileResult]
sortFileResults settings fileResults =
  if sortDescending settings
  then case sortResultsBy settings of
         SortByFileName -> reverse $ sortBy sortFileResultsByName fileResults
         SortByFileType -> reverse $ sortBy sortFileResultsByType fileResults
         _              -> reverse $ sortBy sortFileResultsByPath fileResults
  else case sortResultsBy settings of
         SortByFileName -> sortBy sortFileResultsByName fileResults
         SortByFileType -> sortBy sortFileResultsByType fileResults
         _              -> sortBy sortFileResultsByPath fileResults

doFind :: FindSettings -> IO [FileResult]
doFind settings = do
  fileResults <- getFileResults settings
  return $ sortFileResults settings fileResults
