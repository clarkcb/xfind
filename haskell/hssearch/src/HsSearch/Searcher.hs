module HsSearch.Searcher
    (
      doSearch
    , doSearchFiles
    , filterFile
    , getSearchDirs
    , getSearchFiles
    , isArchiveSearchFile
    , isSearchDir
    , isSearchFile
    , searchContents
    , searchLines
    ) where

import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (catMaybes)
import Text.Regex.PCRE

import HsSearch.FileTypes
import HsSearch.FileUtil
import HsSearch.SearchFile
import HsSearch.SearchResult
import HsSearch.SearchSettings


isSearchDir :: SearchSettings -> FilePath -> Bool
isSearchDir settings d = all ($d) tests
  where tests :: [FilePath -> Bool]
        tests = [ \x -> null inPatterns
                         || any (\p -> x =~ p :: Bool) inPatterns
                , \x -> null outPatterns
                         || all (\p -> not $ x =~ p :: Bool) outPatterns
                , \x -> not (isHiddenFilePath x) || includeHidden
                ]
        inPatterns = inDirPatterns settings
        outPatterns = outDirPatterns settings
        includeHidden = not $ excludeHidden settings

getSearchDirs :: SearchSettings -> IO [FilePath]
getSearchDirs settings = do
  isStartPathDir <- isDirectory $ startPath settings
  if isStartPathDir
    then getDirectories
    else return [getParentPath (startPath settings)]
  where getDirectories :: IO [FilePath]
        getDirectories = do
          dirs <- getRecursiveDirectories $ startPath settings
          return $ filter (isSearchDir settings) $ startPath settings : dirs

isSearchFile :: SearchSettings -> FilePath -> Bool
isSearchFile settings fp = all ($fp) tests
  where tests :: [FilePath -> Bool]
        tests = [ \x -> null inExts
                         || hasInExt x
                , \x -> null outExts
                         || not (hasOutExt x)
                , \x -> null inPatterns
                         || any (\p -> x =~ p :: Bool) inPatterns
                , \x -> null outPatterns
                         || all (\p -> not $ x =~ p :: Bool) outPatterns
                , \x -> null inTypes
                         || (getFileTypeForName fp) `elem` inTypes
                , \x -> null outTypes
                         || (getFileTypeForName fp) `elem` outTypes
                , \x -> not (isHiddenFilePath x) || includeHidden
                ]
        inExts = inExtensions settings
        hasInExt f | null inExts = True
                   | otherwise   = any (hasExtension f) inExts
        outExts = outExtensions settings
        hasOutExt f | null outExts = False
                    | otherwise    = any (hasExtension f) outExts
        inPatterns = inFilePatterns settings
        outPatterns = outFilePatterns settings
        inTypes = inFileTypes settings
        outTypes = outFileTypes settings
        includeHidden = not $ excludeHidden settings

isArchiveSearchFile :: SearchSettings -> FilePath -> Bool
isArchiveSearchFile settings fp = all ($fp) tests
  where tests :: [FilePath -> Bool]
        tests = [ \x -> null inExts
                         || hasInExt x
                , \x -> null outExts
                         || not (hasOutExt x)
                , \x -> null inPatterns
                         || any (\p -> x =~ p :: Bool) inPatterns
                , \x -> null outPatterns
                         || all (\p -> not $ x =~ p :: Bool) outPatterns
                , \x -> not (isHiddenFilePath x) || includeHidden
                ]
        inExts = inArchiveExtensions settings
        hasInExt f | null inExts = True
                   | otherwise   = any (hasExtension f) inExts
        outExts = outArchiveExtensions settings
        hasOutExt f | null outExts = False
                    | otherwise    = any (hasExtension f) outExts
        inPatterns = inArchiveFilePatterns settings
        outPatterns = outArchiveFilePatterns settings
        includeHidden = not $ excludeHidden settings

filterFile :: SearchSettings -> SearchFile -> Bool
filterFile settings sf | isArchiveFile sf = includeArchiveFile sf
                       | otherwise        = includeFile sf
  where includeArchiveFile f = searchArchives settings &&
                               isArchiveSearchFile settings (searchFilePath f)
        includeFile f = not (archivesOnly settings) &&
                        isSearchFile settings (searchFilePath f)

getSearchFiles :: SearchSettings -> [FilePath] -> IO [FilePath]
getSearchFiles settings dirs = do
  isStartPathDir <- isDirectory $ startPath settings
  files <- if isStartPathDir
             then concat `liftM` mapM getDirectoryFiles dirs
             else return [startPath settings]
  fileTypes <- getFileTypes files
  let makeSearchFile (f,t) = SearchFile { searchFileContainers=[]
                                        , searchFilePath=f
                                        , searchFileType=t }
  let filesWithTypes = zipWith (curry makeSearchFile) files fileTypes
  let searchableFiles = filter isSearchableFile filesWithTypes
  return $ map searchFilePath (filter (filterFile settings) searchableFiles)

searchBinaryFile :: SearchSettings -> FilePath -> IO [SearchResult]
searchBinaryFile settings f = do
  blobEither <- getFileByteString f
  case blobEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right blob) -> return $ addFilePath (searchBlob settings blob)
  where addFilePath = map (\r -> r {filePath=f})

searchBlob :: SearchSettings -> B.ByteString -> [SearchResult]
searchBlob settings blob =
  concatMap (searchBlobForPattern settings blob) (searchPatterns settings)

searchBlobForPattern :: SearchSettings -> B.ByteString -> String -> [SearchResult]
searchBlobForPattern settings blob = patternResults
  where lineMatchIndices :: String -> [(Int,Int)]
        lineMatchIndices p = if firstMatch settings
                               then take 1 $ matchIndices blob p
                               else matchIndices blob p
        patternResults :: String -> [SearchResult]
        patternResults p = map (resultFromPatternMatchIndices p) (lineMatchIndices p)
        resultFromPatternMatchIndices :: String -> (Int, Int) -> SearchResult
        resultFromPatternMatchIndices p ix =
          blankSearchResult { searchPattern=p
                            , lineNum=0
                            , matchStartIndex=fst ix + 1
                            , matchEndIndex=snd ix + 1
                            , line=B.empty
                            }

searchTextFile :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFile settings f =
  if multiLineSearch settings
    then searchTextFileContents settings f
    else searchTextFileLines settings f

matchOffsetsAndLengths :: B.ByteString -> String -> [(MatchOffset,MatchLength)]
matchOffsetsAndLengths s p = getAllMatches $ s =~ p :: [(MatchOffset,MatchLength)]

matchIndices :: B.ByteString -> String -> [(Int,Int)]
matchIndices s p = map (\(x,y) -> (x, x+y)) (matchOffsetsAndLengths s p)

linesMatch :: [B.ByteString] -> [String] -> [String] -> Bool
linesMatch [] _ _ = False
linesMatch _ [] [] = True
linesMatch ls lsInPatterns lsOutPatterns = inPatternMatches && not outPatternMatches
  where inPatternMatches = null lsInPatterns || anyMatchesAnyPattern ls lsInPatterns
        outPatternMatches = not (null lsOutPatterns) && anyMatchesAnyPattern ls lsOutPatterns

anyMatchesAnyPattern :: [B.ByteString] -> [String] -> Bool
anyMatchesAnyPattern ls = any (anyMatchesPattern ls)

matchesPattern :: B.ByteString -> String -> Bool
matchesPattern l p = l =~ p

anyMatchesPattern :: [B.ByteString] -> String -> Bool
anyMatchesPattern ls p = any (\l -> matchesPattern l p) ls

matchesAnyPattern :: B.ByteString -> [String] -> Bool
matchesAnyPattern l ps = any (\p -> matchesPattern l p) ps

searchTextFileContents :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileContents settings f = do
  contentsEither <- getFileByteString f
  case contentsEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right contents) -> return $ addFilePath (searchContents settings contents)
  where addFilePath = map (\r -> r {filePath=f})

searchContents :: SearchSettings -> B.ByteString -> [SearchResult]
searchContents settings contents =
  concatMap (searchContentsForPattern settings contents) (searchPatterns settings)

searchContentsForPattern :: SearchSettings -> B.ByteString -> String -> [SearchResult]
searchContentsForPattern settings contents = patternResults
  where patternResults p = catMaybes (maybePatternResults p)
        maybePatternResults p = map (maybeResultFromPatternMatchIndices p) (firstOrAllIndices p)
        firstOrAllIndices p = if firstMatch settings
                              then take 1 (matchIndices contents p)
                              else matchIndices contents p
        newlineIndices =  BC.findIndices (=='\n') contents
        startLineIndices = 0 : map (+1) newlineIndices
        startLineIndex idx = case takeWhile (<=idx) startLineIndices of
                             [] -> 0
                             x  -> last x
        endLineIndex idx   = case dropWhile (<=idx) startLineIndices of
                             [] -> BC.length contents - 1
                             x  -> head x - 1
        lineLength i = endLineIndex i - startLineIndex i
        lineAtIndex i = B.take (lineLength i) $ B.drop (startLineIndex i) contents
        countNewlines s = BC.length $ BC.filter (=='\n') s
        linesBeforeNum = linesBefore settings
        takeRight n = reverse . take n . reverse
        linesBeforeIndices i n = (takeRight n . takeWhile (<i)) startLineIndices
        getLinesBefore i n = map lineAtIndex (linesBeforeIndices i n)
        beforeLns i | linesBeforeNum == 0 = []
                    | otherwise = getLinesBefore (startLineIndex i) linesBeforeNum
        linesAfterNum = linesAfter settings
        linesAfterIndices i n = (take n . dropWhile (<=i)) startLineIndices
        getLinesAfter i n = map lineAtIndex (linesAfterIndices i n)
        afterLns i | linesAfterNum == 0 = []
                   | otherwise = getLinesAfter (startLineIndex i) linesAfterNum
        checkBefore = linesBefore settings > 0
        beforeLinesMatch bs =
          not checkBefore
          || linesMatch bs (inLinesBeforePatterns settings) (outLinesBeforePatterns settings)
        checkAfter = linesAfter settings > 0
        afterLinesMatch as =
          not checkAfter
          || linesMatch as (inLinesAfterPatterns settings) (outLinesAfterPatterns settings)
        maybeResultFromPatternMatchIndices :: String -> (Int, Int) -> Maybe SearchResult
        maybeResultFromPatternMatchIndices p ix =
          if beforeLinesMatch bs && afterLinesMatch as
          then
            Just blankSearchResult { searchPattern=p
                                   , lineNum=lineCount
                                   , matchStartIndex=msi
                                   , matchEndIndex=mei
                                   , line=lineAtIndex (fst ix)
                                   , beforeLines=bs
                                   , afterLines=as
                                   }
          else Nothing
          where lineCount = countNewlines (B.take (fst ix) contents) + 1
                sli = startLineIndex (fst ix)
                msi = fst ix - sli + 1
                mei = snd ix - sli + 1
                bs = beforeLns (fst ix)
                as = afterLns (fst ix)

searchTextFileLines :: SearchSettings -> FilePath -> IO [SearchResult]
searchTextFileLines settings f = do
  fileLinesEither <- getFileLines f
  case fileLinesEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right fileLines) -> return $ addFilePath (searchLines settings fileLines)
  where addFilePath = map (\r -> r {filePath=f})

searchLines :: SearchSettings -> [B.ByteString] -> [SearchResult]
searchLines settings lineList = recSearchLines settings [] lineList 0 []

recSearchLines :: SearchSettings -> [B.ByteString] -> [B.ByteString] -> Int -> [SearchResult] -> [SearchResult]
recSearchLines settings beforeList lst num results =
  case lst of
    []     -> results
    (l:ls) -> recSearchLines settings (newBefore l) ls (num + 1) (updatedResults l)
  where beforeNum = linesBefore settings
        newBefore l | beforeNum == 0 = []
                    | length beforeList == beforeNum = tail beforeList ++ [l]
                    | otherwise = beforeList ++ [l]
        afterNum = linesAfter settings
        afterToPatterns = linesAfterToPatterns settings
        afterUntilPatterns = linesAfterUntilPatterns settings
        checkAfterTo = not (null afterToPatterns)
        checkAfterUntil = not (null afterUntilPatterns)
        notMatchesAnyPattern ps l = not $ matchesAnyPattern l ps
        afterToCount =
          if checkAfterTo
          then length (takeWhile (notMatchesAnyPattern afterToPatterns) (tail lst)) + 1
          else 0
        afterUntilCount =
          if checkAfterUntil
          then length (takeWhile (notMatchesAnyPattern afterUntilPatterns) (tail lst))
          else 0
        afterList
          | checkAfterTo = take afterToCount (tail lst)
          | checkAfterUntil = take afterUntilCount (tail lst)
          | otherwise = take afterNum (tail lst)
        updatedResults l = results ++ newResults l
        newResults l = concatMap (searchNextPattern l) filteredPatterns
        searchNextPattern l = searchLineForPattern settings (num + 1) beforeList l afterList
        filteredPatterns = if firstMatch settings
                           then filter firstMatchNotMet patterns
                           else patterns
        firstMatchNotMet p = not (any (\r -> searchPattern r == p) results)
        patterns = searchPatterns settings

searchLineForPattern :: SearchSettings -> Int -> [B.ByteString] -> B.ByteString -> [B.ByteString] -> String -> [SearchResult]
searchLineForPattern settings num bs l as = patternResults
  where checkBefore = linesBefore settings > 0
        beforeLinesMatch =
          not checkBefore
          || linesMatch bs (inLinesBeforePatterns settings) (outLinesBeforePatterns settings)
        checkAfter = linesAfter settings > 0
        afterLinesMatch =
          not checkAfter
          || linesMatch as (inLinesAfterPatterns settings) (outLinesAfterPatterns settings)
        lineMatchIndices :: String -> [(Int,Int)]
        lineMatchIndices p = if beforeLinesMatch && afterLinesMatch
                             then if firstMatch settings
                                  then take 1 $ matchIndices l p
                                  else matchIndices l p
                             else []
        patternResults :: String -> [SearchResult]
        patternResults p = map (resultFromPatternMatchIndices p) (lineMatchIndices p)
        resultFromPatternMatchIndices :: String -> (Int, Int) -> SearchResult
        resultFromPatternMatchIndices p ix =
          blankSearchResult { searchPattern=p
                            , lineNum=num
                            , matchStartIndex=fst ix + 1
                            , matchEndIndex=snd ix + 1
                            , line=l
                            , beforeLines=bs
                            , afterLines=as
                            }

doSearchFile :: SearchSettings -> (FilePath,FileType) -> IO [SearchResult]
doSearchFile settings ft =
  case snd ft of
    Binary -> searchBinaryFile settings $ fst ft
    filetype | filetype `elem` [Code, Text, Xml] -> searchTextFile settings $ fst ft
    _ -> return []

doSearchFiles :: SearchSettings -> [FilePath] -> IO [SearchResult]
doSearchFiles settings searchFiles = do
  fileTypes <- getFileTypes searchFiles
  results <- mapM (doSearchFile settings) (zip searchFiles fileTypes)
  return $ concat results

doSearch :: SearchSettings -> IO [SearchResult]
doSearch settings = do
  searchDirs <- getSearchDirs settings
  searchFiles <- getSearchFiles settings searchDirs
  doSearchFiles settings searchFiles
