module HsFind.Finder
    (
      doFind
    , filterToFileResult
    , getFileResults
    , isMatchingArchiveFilePath
    , isMatchingDirPath
    , isMatchingFilePath
    , validateSettings
    ) where

import Control.Monad (forM, filterM)
import Data.Char (toLower)
import Data.List (partition, sortBy, zipWith4)
import Data.Maybe (fromJust, isJust, isNothing)

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (dropFileName, splitPath, takeFileName)
import Text.Regex.PCRE ( (=~) )
import Data.Time (UTCTime)

import HsFind.FileTypes (FileType(..), JsonFileType, getJsonFileTypes, getFileTypesFromJsonFileTypes)
import HsFind.FileUtil
    (expandPath, filterDirectories, filterFiles, filterOutSymlinks, hasExtension, isHiddenFilePath,
    getNonDotDirectoryContents, getFileSizes, getModificationTimes, partitionDirsAndFiles,
    pathExists, pathsExist)
import HsFind.FileResult
    (FileResult(..), newFileResult, newFileResultWithSizeAndLastMod)
import HsFind.FindSettings


-- TODO: need to add validation for path existence, will require IO
validateSettings :: FindSettings -> Maybe String
validateSettings settings = recValidateSettings settings validators []
  where recValidateSettings :: FindSettings -> [FindSettings -> [String]] -> [String] -> Maybe String
        recValidateSettings settings validators errs = do
          case errs of
            [] -> case validators of
                    [] -> Nothing
                    (v:vs) -> recValidateSettings settings vs (v settings)
            _ -> Just $ head errs
        validators = [ \s -> ["Startpath not defined" | null (paths s)]
                     , \s -> ["Invalid range for mindepth and maxdepth" | maxDepth s > 0 && maxDepth s < minDepth s]
                     , \s -> ["Invalid range for minlastmod and maxlastmod" | invalidLastModRange s]
                     , \s -> ["Invalid range for minsize and maxsize" | maxSize s > 0 && maxSize s < minSize s]
                     ]
        invalidLastModRange s = isJust (maxLastMod s) && isJust (maxLastMod s) && Just (maxLastMod s) < Just (minLastMod s)

getHiddenFilePathTests :: FindSettings -> [FilePath -> Bool]
getHiddenFilePathTests settings =
  hiddenPathTests
  where hiddenPathTests | includeHidden settings = []
                        | otherwise = [not . isHiddenFilePath]

matchesHiddenFilePathTests :: [FilePath -> Bool] -> FilePath -> Bool
matchesHiddenFilePathTests [] _ = True
matchesHiddenFilePathTests tests f = all ($f) tests

getDirPathTests :: FindSettings -> [FilePath -> Bool]
getDirPathTests settings = hiddenPathTests ++ inPatternTests ++ outPatternTests
  where hiddenPathTests = getHiddenFilePathTests settings
        inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> fp =~ p :: Bool) inPatterns]
        outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ fp =~ p :: Bool) outPatterns]
        inPatterns = inDirPatterns settings
        outPatterns = outDirPatterns settings

matchesDirPathTests :: [FilePath -> Bool] -> FilePath -> Bool
matchesDirPathTests [] _ = True
matchesDirPathTests tests d = all ($d) tests

isMatchingDirPath :: FindSettings -> FilePath -> Bool
isMatchingDirPath settings = matchesDirPathTests dirPathTests
  where dirPathTests :: [FilePath -> Bool]
        dirPathTests = getDirPathTests settings

getArchiveFilePathTests :: FindSettings -> [FilePath -> Bool]
getArchiveFilePathTests settings =
  hiddenPathTests ++ inExtTests ++ outExtTests ++ inPatternTests ++ outPatternTests
  where hiddenPathTests = getHiddenFilePathTests settings
        inExtTests      | null inExts = []
                        | otherwise = [\fp -> any (hasExtension fp) inExts]
        outExtTests     | null outExts = []
                        | otherwise = [\fp -> not $ any (hasExtension fp) outExts]
        inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> takeFileName fp =~ p :: Bool) inPatterns]
        outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ takeFileName fp =~ p :: Bool) outPatterns]
        inExts = inArchiveExtensions settings
        outExts = outArchiveExtensions settings
        inPatterns = inArchiveFilePatterns settings
        outPatterns = outArchiveFilePatterns settings

matchesArchiveFilePathTests :: [FilePath -> Bool] -> FilePath -> Bool
matchesArchiveFilePathTests [] _ = True
matchesArchiveFilePathTests tests f = all ($f) tests

isMatchingArchiveFilePath :: FindSettings -> FilePath -> Bool
isMatchingArchiveFilePath settings = matchesArchiveFilePathTests archiveFilePathTests
  where archiveFilePathTests :: [FilePath -> Bool]
        archiveFilePathTests = getArchiveFilePathTests settings

getFilePathTests :: FindSettings -> [FilePath -> Bool]
getFilePathTests settings =
  hiddenPathTests ++ inExtTests ++ outExtTests ++ inPatternTests ++ outPatternTests
  where hiddenPathTests = getHiddenFilePathTests settings
        inExtTests      | null inExts = []
                        | otherwise = [\fp -> any (hasExtension fp) inExts]
        outExtTests     | null outExts = []
                        | otherwise = [\fp -> not $ any (hasExtension fp) outExts]
        inPatternTests  | null inPatterns = []
                        | otherwise = [\fp -> any (\p -> takeFileName fp =~ p :: Bool) inPatterns]
        outPatternTests | null outPatterns = []
                        | otherwise = [\fp -> all (\p -> not $ takeFileName fp =~ p :: Bool) outPatterns]
        inExts = inExtensions settings
        outExts = outExtensions settings
        inPatterns = inFilePatterns settings
        outPatterns = outFilePatterns settings

matchesFilePathTests :: [FilePath -> Bool] -> FilePath -> Bool
matchesFilePathTests [] _ = True
matchesFilePathTests tests f = all ($f) tests

isMatchingFilePath :: FindSettings -> FilePath -> Bool
isMatchingFilePath settings = matchesFilePathTests filePathTests
  where filePathTests :: [FilePath -> Bool]
        filePathTests = getFilePathTests settings

getArchiveFileResultTests :: FindSettings -> [FileResult -> Bool]
getArchiveFileResultTests settings =
  archiveFilePathTests
  where archiveFilePathTests = map (. fileResultPath) $ getArchiveFilePathTests settings

matchesArchiveFileResultTests :: [FileResult -> Bool] -> FileResult -> Bool
matchesArchiveFileResultTests [] _ = True
matchesArchiveFileResultTests tests fr = all ($fr) tests

getFileTypeTests :: FindSettings -> [FileType -> Bool]
getFileTypeTests settings =
  archiveFileTypeTests ++ inFileTypeTests ++ outFileTypeTests
  where archiveFileTypeTests = [\ft -> if ft == Archive then includeArchives settings else not (archivesOnly settings)]
        inFileTypeTests  | null inTypes = []
                         | otherwise = [(`elem` inTypes)]
        outFileTypeTests | null outTypes = []
                         | otherwise = [(`notElem` outTypes)]
        inTypes = inFileTypes settings
        outTypes = outFileTypes settings

matchesFileTypeTests :: [FileType -> Bool] -> FileType -> Bool
matchesFileTypeTests [] _ = True
matchesFileTypeTests tests ft = all ($ft) tests

getFileSizeTests :: FindSettings -> [Integer -> Bool]
getFileSizeTests settings =
  maxSizeTests ++ minSizeTests
  where maxSizeTests | maxSize settings == 0 = []
                     | otherwise = [\i -> i <= maxSize settings]
        minSizeTests | minSize settings == 0 = []
                     | otherwise = [\i -> i >= minSize settings]

getLastModTests :: FindSettings -> [Maybe UTCTime -> Bool]
getLastModTests settings =
  maxLastModTests ++ minLastModTests
  where maxLastModTests | isNothing (maxLastMod settings) = []
                        | otherwise = [\lastMod -> fromJust lastMod <= fromJust (maxLastMod settings)]
        minLastModTests | isNothing (minLastMod settings) = []
                        | otherwise = [\lastMod -> fromJust lastMod >= fromJust (minLastMod settings)]

getFileResultTests :: FindSettings -> [FileResult -> Bool]
getFileResultTests settings =
  filePathTests ++ fileTypeTests ++ fileSizeTests ++ lastModTests
  where filePathTests = map (. fileResultPath) $ getFilePathTests settings
        fileTypeTests = map (. fileResultType) $ getFileTypeTests settings
        fileSizeTests = map (. fileResultSize) $ getFileSizeTests settings
        lastModTests = map (. fileLastMod) $ getLastModTests settings

matchesFileResultTests :: [FileResult -> Bool] -> FileResult -> Bool
matchesFileResultTests [] _ = True
matchesFileResultTests tests fr = all ($fr) tests

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

getRecursiveFilePaths :: FindSettings -> FilePath -> IO [FilePath]
getRecursiveFilePaths settings dir = do
  accRecursiveFilePaths dir 1 minDepth' maxDepth' dirPathFilter hiddenPathFilter
  where minDepth' = minDepth settings
        maxDepth' = if recursive settings then maxDepth settings else 1
        dirPathFilter = matchesDirPathTests $ getDirPathTests settings
        hiddenPathFilter = matchesHiddenFilePathTests $ getHiddenFilePathTests settings
        accRecursiveFilePaths :: FilePath -> Integer -> Integer -> Integer -> (FilePath -> Bool) -> (FilePath -> Bool) -> IO [FilePath]
        accRecursiveFilePaths dir depth minDepth maxDepth dirPathFilter hiddenPathFilter = do
          allPaths <- getNonDotDirectoryContents dir
          (dirPaths, filePaths) <- partitionDirsAndFiles allPaths
          filteredDirPathsBySymlinks <- if followSymlinks settings
                                        then return dirPaths
                                        else filterOutSymlinks dirPaths
          let filteredDirPaths = if maxDepth < 1 || depth <= maxDepth
                                 then filter dirPathFilter filteredDirPathsBySymlinks
                                 else []
          filteredFilePathsBySymlinks <- if followSymlinks settings
                                         then return filePaths
                                         else filterOutSymlinks filePaths
          let filteredFilePaths = if depth >= minDepth && (maxDepth < 1 || depth <= maxDepth)
                                  then filter hiddenPathFilter filteredFilePathsBySymlinks
                                  else []
          subDirPaths <- forM filteredDirPaths $ \d -> accRecursiveFilePaths d (depth + 1) minDepth maxDepth dirPathFilter hiddenPathFilter
          return $ filteredFilePaths ++ concat subDirPaths

getFileResults :: FindSettings -> IO [FileResult]
getFileResults settings = do
  -- TODO: need to handle case where paths are files, not just directories
  pathDirs <- filterDirectories (paths settings)
  pathFiles <- filterFiles (paths settings)
  pathLists <- forM pathDirs $ \path ->
    getRecursiveFilePaths settings path
  let allPaths = concat pathLists ++ pathFiles
  jsonFileTypes <- getJsonFileTypes
  let allFileTypes = getFileTypesFromJsonFileTypes jsonFileTypes allPaths
  let allPathsAndTypes = zip allPaths allFileTypes
  let fileTypesFilter = matchesFileTypeTests $ getFileTypeTests settings
  let filteredPathsAndTypes = filter (\(_, ft) -> fileTypesFilter ft) allPathsAndTypes
  let (filteredPaths, filteredTypes) = unzip filteredPathsAndTypes
  filteredFileSizes <- if needFileSizes settings
                       then getFileSizes filteredPaths
                       else return $ replicate (length filteredPaths) 0
  filteredLastMods <- if needLastMods settings
                      then do
                        filteredLastMods <- getModificationTimes filteredPaths
                        return $ map Just filteredLastMods
                      else return $ replicate (length filteredPaths) Nothing
  let archiveFileResultsFilter = matchesArchiveFileResultTests $ getArchiveFileResultTests settings
  let fileResultsFilter = matchesFileResultTests $ getFileResultTests settings
  let fileResults = zipWith4 (curry4 getFileResultWithSizeAndLastMod) filteredPaths filteredTypes filteredFileSizes filteredLastMods
  let (archiveFileResults, nonArchiveFileResults) = partition (\fr -> fileResultType fr == Archive) fileResults
  let filteredArchiveFileResults =  if not (includeArchives settings)
                                    then []
                                    -- else filter archiveFileResultsFilter $ traceFileResults "archiveFileResults" archiveFileResults
                                    else filter archiveFileResultsFilter archiveFileResults
  let filteredNonArchiveFileResults = if archivesOnly settings
                                      then []
                                      -- else filter fileResultsFilter $ traceFileResults "nonArchiveFileResults" nonArchiveFileResults
                                      else filter fileResultsFilter nonArchiveFileResults
  -- return $ traceFileResults "filteredArchiveFileResults" filteredArchiveFileResults ++
  --          traceFileResults "filteredNonArchiveFileResults" filteredNonArchiveFileResults
  return $  filteredArchiveFileResults ++ filteredNonArchiveFileResults

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

getFoundPaths :: [FilePath] -> IO (Either String [FilePath])
getFoundPaths paths = do
  foundPaths <- filterM pathExists paths
  let notFoundPaths = filter (`notElem` foundPaths) paths
  expandedPaths <- mapM expandPath notFoundPaths
  foundExpandedPaths <- filterM pathExists expandedPaths
  let allFoundPaths = foundPaths ++ foundExpandedPaths
  if length allFoundPaths == length paths then do
    return $ Right allFoundPaths
  else do
    return $ Left "Startpath not found"

doFind :: FindSettings -> IO (Either String [FileResult])
doFind settings = do
  foundPathsEither <- getFoundPaths (paths settings)
  case foundPathsEither of
    Left errMsg -> return $ Left errMsg
    Right foundPaths -> do
      let settings' = settings { paths = foundPaths }
      fileResults <- getFileResults settings'
      return $ Right $ sortFileResults settings' fileResults
