module HsFind.Finder
    (
      doFind
    , filterFile
    , filterToFileResult
    , getFileResults
    , isMatchingArchiveFile
    , isMatchingDir
    , isMatchingFile
    , validateSettings
    ) where

import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (sortBy, zipWith4)
import Data.Maybe (isJust, isNothing)

import System.FilePath (dropFileName, splitPath, takeFileName)
import Text.Regex.PCRE ( (=~) )
import Data.Time (UTCTime)

import HsFind.FileTypes (FileType(..), JsonFileType, getFileTypes, getJsonFileTypes, fileTypeFromJsonFileTypes)
import HsFind.FileUtil
    (hasExtension, isHiddenFilePath, getRecursiveFilteredContents, getFileSizes, getModificationTimes)
import HsFind.FileResult
    (FileResult(..), isArchiveFile, newFileResult, newFileResultWithSizeAndLastMod)
import HsFind.FindSettings


-- TODO: need to also validate existence of paths, but since IO is involved, might have to approach differently
validateSettings :: FindSettings -> [String]
validateSettings settings = concatMap ($settings) validators
  where validators = [ \s -> ["Startpath not defined" | null (paths s)]
                     , \s -> ["Invalid maxsize" | maxSize s < 0]
                     , \s -> ["Invalid minsize" | maxSize s < 0]
                     ]

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
  archiveFileTests ++ hiddenPathTests ++ inExtTests ++ outExtTests ++ inPatternTests ++
    outPatternTests ++ inFileTypeTests ++ outFileTypeTests
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
        archiveFileTests = getArchiveFileTests settings
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

getFileResultTests :: FindSettings -> [FileResult -> Bool]
getFileResultTests settings =
  maxSizeTests ++ minSizeTests ++ maxLastModTests ++ minLastModTests
  where maxSizeTests    | maxSize settings == 0 = []
                        | otherwise = [\fr -> fileResultSize fr < maxSize settings]
        minSizeTests    | minSize settings == 0 = []
                        | otherwise = [\fr -> fileResultSize fr > minSize settings]
        maxLastModTests | isNothing (maxLastMod settings) = []
                        | otherwise = [\fr -> fileLastMod fr < maxLastMod settings]
        minLastModTests | isNothing (minLastMod settings) = []
                        | otherwise = [\fr -> fileLastMod fr > minLastMod settings]

matchesFileResultTests :: [FileResult -> Bool] -> FileResult -> Bool
matchesFileResultTests tests fr = all ($fr) tests

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

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a,b,c,d)

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f ~(a,b,c,d) = f a b c d

getFileResultWithSizeAndLastMod :: (FilePath,FileType,Integer,Maybe UTCTime) -> FileResult
getFileResultWithSizeAndLastMod = uncurry4 newFileResultWithSizeAndLastMod

getFileResults :: FindSettings -> IO [FileResult]
getFileResults settings = do
  let dirTests = getDirTests settings
  jsonFileTypes <- getJsonFileTypes
  let fileTests = getAllFileTests settings jsonFileTypes
  pathLists <- forM (paths settings) $ \path ->
    getRecursiveFilteredContents path (matchesDirTests dirTests) (matchesFileTests fileTests)
  let allPaths = concat pathLists
  allFileTypes <- getFileTypes allPaths
  allFileSizes <- if maxSize settings > 0 || minSize settings > 0 || sortResultsBy settings == SortByFileSize
                  then getFileSizes allPaths
                  else return $ replicate (length allPaths) 0
  allLastMods <- if isJust (maxLastMod settings) || isJust (minLastMod settings) || sortResultsBy settings == SortByLastMod
                 then do
                  _allLastMods <- getModificationTimes allPaths
                  return $ map Just _allLastMods
                 else return $ replicate (length allPaths) Nothing
  let fileResultsTests = getFileResultTests settings
  let fileResults = zipWith4 (curry4 getFileResultWithSizeAndLastMod) allPaths allFileTypes allFileSizes allLastMods
  if not (includeArchives settings) && Archive `elem` allFileTypes
  then return $ filter (not . isArchiveFile) fileResults
  else return $ filter (matchesFileResultTests fileResultsTests) fileResults

compareStrings :: FindSettings -> String -> String -> Ordering
compareStrings settings s1 s2 =
  if sortCaseInsensitive settings
    then compare (lower s1) (lower s2)
    else compare s1 s2
  where lower = map toLower

comparePaths :: FindSettings -> String -> String -> Ordering
comparePaths settings p1 p2 =
  if sortCaseInsensitive settings
    then compare (map lower elems1) (map lower elems2)
    else compare elems1 elems2
  where lower = map toLower
        elems1 = splitPath p1
        elems2 = splitPath p2

sortFileResultsByPath :: FindSettings -> FileResult -> FileResult -> Ordering
sortFileResultsByPath settings fr1 fr2 =
  if pcmp == EQ
  then compareStrings settings f1 f2
  else comparePaths settings p1 p2
  where p1 = dropFileName (fileResultPath fr1)
        p2 = dropFileName (fileResultPath fr2)
        f1 = takeFileName (fileResultPath fr1)
        f2 = takeFileName (fileResultPath fr2)
        pcmp = compareStrings settings p1 p2

sortFileResultsByName :: FindSettings -> FileResult -> FileResult -> Ordering
sortFileResultsByName settings fr1 fr2 =
  if fcmp == EQ
  then comparePaths settings p1 p2
  else compareStrings settings f1 f2
  where p1 = dropFileName (fileResultPath fr1)
        p2 = dropFileName (fileResultPath fr2)
        f1 = takeFileName (fileResultPath fr1)
        f2 = takeFileName (fileResultPath fr2)
        fcmp = compareStrings settings f1 f2

sortFileResultsBySize :: FindSettings -> FileResult -> FileResult -> Ordering
sortFileResultsBySize settings fr1 fr2 =
  if s1 == s2
  then sortFileResultsByPath settings fr1 fr2
  else compare s1 s2
  where s1 = fileResultSize fr1
        s2 = fileResultSize fr2

sortFileResultsByType :: FindSettings -> FileResult -> FileResult -> Ordering
sortFileResultsByType settings fr1 fr2 =
  if t1 == t2
  then sortFileResultsByPath settings fr1 fr2
  else compare t1 t2
  where t1 = fileResultType fr1
        t2 = fileResultType fr2

sortFileResultsByLastMod :: FindSettings -> FileResult -> FileResult -> Ordering
sortFileResultsByLastMod settings fr1 fr2 =
  if m1 == m2
  then sortFileResultsByPath settings fr1 fr2
  else compare m1 m2
  where m1 = fileLastMod fr1
        m2 = fileLastMod fr2

doSortByFileResults :: FindSettings -> [FileResult] -> [FileResult]
doSortByFileResults settings fileResults =
  case sortResultsBy settings of
   SortByFileName -> sortBy (sortFileResultsByName settings) fileResults
   SortByFileSize -> sortBy (sortFileResultsBySize settings) fileResults
   SortByFileType -> sortBy (sortFileResultsByType settings) fileResults
   SortByLastMod  -> sortBy (sortFileResultsByLastMod settings) fileResults
   _              -> sortBy (sortFileResultsByPath settings) fileResults

sortFileResults :: FindSettings -> [FileResult] -> [FileResult]
sortFileResults settings fileResults =
  if sortDescending settings
    then reverse $ doSortByFileResults settings fileResults
    else doSortByFileResults settings fileResults

doFind :: FindSettings -> IO [FileResult]
doFind settings = do
  fileResults <- getFileResults settings
  return $ sortFileResults settings fileResults
