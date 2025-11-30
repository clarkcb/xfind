module HsFind.Finder
  ( doFind
  , filterToFileResult
  , formatMatchingDirs
  , formatMatchingFiles
  , getFinder
  , isMatchingArchiveFilePath
  , isMatchingDirPath
  , isMatchingFilePath
  , validateFindSettings
  ) where

import Control.Monad (forM, filterM)
import Data.List (nub, partition, sort, zipWith4)
import Data.Maybe (isJust)

import System.FilePath (takeDirectory)
import Data.Time (UTCTime)

import HsFind.FileTypes (FileType(..), JsonFileType, getJsonFileTypes, getFileTypesFromJsonFileTypes)
import HsFind.FileUtil
    (expandPath, filterOutSymlinks, getFileSizes, getModificationTimes, getNonDotDirectoryContents,
     partitionDirsAndFiles, partitionExisting, pathExists)
import HsFind.FileResult
    (FileResult(..), formatDirectory, formatFileResult, newFileResult, newFileResultWithSizeAndLastMod,
     sortFileResults)
import HsFind.FilterTests (FilterTests(..), getFilterTests)
import HsFind.FindSettings


data Finder = Finder
  { settings :: FindSettings
  , filterTests :: FilterTests
  }

getFinder :: FindSettings -> Finder
getFinder settings = Finder
  { settings = settings
  , filterTests = getFilterTests settings
  }

-- TODO: need to add validation for path existence, will require IO
validateFindSettings :: FindSettings -> Maybe String
validateFindSettings settings =
  if printUsage settings
  then Nothing
  else
    recValidateSettings settings validators []
  where recValidateSettings :: FindSettings -> [FindSettings -> [String]] -> [String] -> Maybe String
        recValidateSettings settings' validators' errs = do
          case errs of
            [] -> case validators' of
                    [] -> Nothing
                    (v:vs) -> recValidateSettings settings' vs (v settings')
            _ -> Just $ head errs
        validators = [ \s -> ["Startpath not defined" | null (paths s)]
                     , \s -> ["Invalid range for mindepth and maxdepth" | maxDepth s > 0 && maxDepth s < minDepth s]
                     , \s -> ["Invalid range for minlastmod and maxlastmod" | invalidLastModRange s]
                     , \s -> ["Invalid range for minsize and maxsize" | maxSize s > 0 && maxSize s < minSize s]
                     ]
        invalidLastModRange s = isJust (maxLastMod s) && isJust (maxLastMod s) && Just (maxLastMod s) < Just (minLastMod s)

matchesFilePathTests :: [FilePath -> Bool] -> FilePath -> Bool
matchesFilePathTests [] _ = True
matchesFilePathTests tests f = all ($f) tests

matchesFileResultTests :: [FileResult -> Bool] -> FileResult -> Bool
matchesFileResultTests [] _ = True
matchesFileResultTests tests fr = all ($fr) tests

matchesFileTypeTests :: [FileType -> Bool] -> FileType -> Bool
matchesFileTypeTests [] _ = True
matchesFileTypeTests tests ft = all ($ft) tests

filterDirByInPatterns :: Finder -> FilePath -> Bool
filterDirByInPatterns finder = matchesFilePathTests inPatternsTests
  where inPatternsTests = dirPathByInPatternsTests $ filterTests finder

filterDirByOutPatterns :: Finder -> FilePath -> Bool
filterDirByOutPatterns finder = matchesFilePathTests outPatternsTests
  where outPatternsTests = dirPathByOutPatternsTests $ filterTests finder

isMatchingDirPath :: Finder -> FilePath -> Bool
isMatchingDirPath finder = matchesFilePathTests dpTests
  where dpTests = dirPathTests $ filterTests finder

isMatchingArchiveFilePath :: Finder -> FilePath -> Bool
isMatchingArchiveFilePath finder = matchesFilePathTests afpTests
  where afpTests = archiveFilePathTests $ filterTests finder

isMatchingFilePath :: Finder -> FilePath -> Bool
isMatchingFilePath finder = matchesFilePathTests fpTests
  where fpTests = filePathTests $ filterTests finder

filterToFileResult :: Finder -> (FilePath,FileType) -> Maybe FileResult
filterToFileResult finder ft =
  if matchesFileTypeTests ftTests (snd ft)
  then Just $ uncurry newFileResult ft
  else Nothing
  where ftTests = fileTypeTests $ filterTests finder

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a,b,c,d)

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f ~(a,b,c,d) = f a b c d

getFileResultWithSizeAndLastMod :: (FilePath,FileType,Integer,Maybe UTCTime) -> FileResult
getFileResultWithSizeAndLastMod = uncurry4 newFileResultWithSizeAndLastMod

getRecursiveFilePaths :: Finder -> FilePath -> IO [FilePath]
getRecursiveFilePaths finder dir = do
  accRecursiveFilePaths dir 1 minDepth' maxDepth' dirPathFilter filePathFilter
  where minDepth' = minDepth ss
        maxDepth' = if recursive ss then maxDepth ss else 1
        dirPathTests' = pathByHiddenTests (filterTests finder) ++ dirPathByOutPatternsTests (filterTests finder)
        dirPathFilter = matchesFilePathTests dirPathTests'
        filePathTests' = filePathTests (filterTests finder)
        filePathFilter = matchesFilePathTests filePathTests'
        accRecursiveFilePaths :: FilePath -> Integer -> Integer -> Integer -> (FilePath -> Bool) -> (FilePath -> Bool) -> IO [FilePath]
        accRecursiveFilePaths dir' depth minDep maxDep dpFilter fpFilter = do
          allPaths <- getNonDotDirectoryContents dir'
          (dirPaths, filePaths) <- partitionDirsAndFiles allPaths
          filteredDirPathsBySymlinks <- if followSymlinks ss
                                        then return dirPaths
                                        else filterOutSymlinks dirPaths
          let filteredDirPaths = if maxDep < 1 || depth <= maxDep
                                 then filter dpFilter filteredDirPathsBySymlinks
                                 else []
          filteredFilePathsBySymlinks <- if followSymlinks ss
                                         then return filePaths
                                         else filterOutSymlinks filePaths
          let filteredFilePaths = if depth >= minDep && (maxDep < 1 || depth <= maxDep)
                                  then filter fpFilter filteredFilePathsBySymlinks
                                  else []
          subDirPaths <- forM filteredDirPaths $ \d -> accRecursiveFilePaths d (depth + 1) minDep maxDep dpFilter fpFilter
          return $ filteredFilePaths ++ concat subDirPaths
        ss = settings finder

getFileResults :: Finder -> IO (Either String [FileResult])
getFileResults finder = do
  (pathDirs, pathFiles) <- partitionDirsAndFiles (paths ss)
  let dirPathTests' = pathByHiddenTests (filterTests finder) ++ dirPathByOutPatternsTests (filterTests finder)
  let filteredPathDirs = filter (matchesFilePathTests dirPathTests') pathDirs
  let filePathTests' = filePathTests (filterTests finder)
  let filteredPathFiles = filter (matchesFilePathTests filePathTests') pathFiles
  if length filteredPathDirs < length pathDirs || length filteredPathFiles < length pathFiles then
    return $ Left "Startpath does not match find settings"
  else do
    pathLists <- forM pathDirs $ \path ->
      getRecursiveFilePaths finder path
    let allPaths = concat pathLists ++ pathFiles
    jsonFileTypes <- getJsonFileTypes
    let allFileTypes = getFileTypesFromJsonFileTypes jsonFileTypes allPaths
    let allPathsAndTypes = zip allPaths allFileTypes
    let fileTypesFilter = matchesFileTypeTests $ fileTypeTests $ filterTests finder
    let filteredPathsAndTypes = filter (\(_, ft) -> fileTypesFilter ft) allPathsAndTypes
    let (filteredPaths, filteredTypes) = unzip filteredPathsAndTypes
    filteredFileSizes <- if needFileSizes ss
                        then getFileSizes filteredPaths
                        else return $ replicate (length filteredPaths) 0
    filteredLastMods <- if needLastMods ss
                        then do
                          filteredLastMods <- getModificationTimes filteredPaths
                          return $ map Just filteredLastMods
                        else return $ replicate (length filteredPaths) Nothing
    let archiveFileResultsFilter = matchesFileResultTests $ archiveFileResultTests $ filterTests finder
    let fileResultsFilter = matchesFileResultTests $ fileResultTests $ filterTests finder
    let fileResults = zipWith4 (curry4 getFileResultWithSizeAndLastMod) filteredPaths filteredTypes filteredFileSizes filteredLastMods
    let (archiveFileResults, nonArchiveFileResults) = partition (\fr -> fileResultType fr == Archive) fileResults
    let filteredArchiveFileResults =  if not (includeArchives ss)
                                      then []
                                      else filter archiveFileResultsFilter archiveFileResults
    let filteredNonArchiveFileResults = if archivesOnly ss
                                        then []
                                        else filter fileResultsFilter nonArchiveFileResults
    return $ Right $ filteredArchiveFileResults ++ filteredNonArchiveFileResults
  where ss = settings finder

getExistingPaths :: [FilePath] -> IO (Either String [FilePath])
getExistingPaths allPaths = do
  (existingPaths, nonExistingPaths) <- partitionExisting allPaths
  expandedPaths <- mapM expandPath nonExistingPaths
  existingExpandedPaths <- filterM pathExists expandedPaths
  let allExistingPaths = existingPaths ++ existingExpandedPaths
  if length allExistingPaths == length allPaths then do
    return $ Right allExistingPaths
  else do
    return $ Left "Startpath not found"

doFind :: Finder -> IO (Either String [FileResult])
doFind finder = do
  existingPathsEither <- getExistingPaths (paths ss)
  case existingPathsEither of
    Left errMsg -> return $ Left errMsg
    Right existingPaths -> do
      let settings' = ss { paths = existingPaths }
      let finder' = finder { settings = settings' }
      fileResultsEither <- getFileResults finder'
      case fileResultsEither of
        Left errMsg -> return $ Left errMsg
        Right fileResults -> return $ Right $ sortFileResults settings' fileResults
  where ss = settings finder

getMatchingDirs :: [FileResult] -> [FilePath]
getMatchingDirs = sort . nub . map (takeDirectory . fileResultPath)

formatMatchingDirs :: FindSettings -> [FileResult] -> String
formatMatchingDirs settings fileResults =
  if not (null matchingDirs) then
    "\nMatching directories (" ++ show (length matchingDirs) ++ "):\n" ++
    unlines matchingDirs
  else "\nMatching directories: 0\n"
  where matchingDirs = map (formatDirectory settings) $ getMatchingDirs fileResults

formatMatchingFiles :: FindSettings -> [FileResult] -> String
formatMatchingFiles settings fileResults =
  if not (null matchingFiles) then
    "\nMatching files (" ++ show (length matchingFiles) ++ "):\n" ++
    unlines matchingFiles
  else "\nMatching files: 0\n"
  where matchingFiles = map (formatFileResult settings) fileResults
