module HsFind.Finder
    (
      doFind
    , doFindFiles
    , filterFile
    , getFindFiles
    , isArchiveFindFile
    , isFindDir
    , isFindFile
    , findContents
    , findLines
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (catMaybes)
import Text.Regex.PCRE

import HsFind.FileTypes
import HsFind.FileUtil
import HsFind.FindFile
import HsFind.FindResult
import HsFind.FindSettings


isFindDir :: FindSettings -> FilePath -> Bool
isFindDir settings d = all ($d) tests
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

isFindFile :: FindSettings -> FilePath -> Bool
isFindFile settings fp = all ($fp) tests
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
                         || getFileTypeForName x `elem` inTypes
                , \x -> null outTypes
                         || getFileTypeForName x `elem` outTypes
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

isArchiveFindFile :: FindSettings -> FilePath -> Bool
isArchiveFindFile settings fp = all ($fp) tests
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

filterFile :: FindSettings -> FindFile -> Bool
filterFile settings sf | isArchiveFile sf = includeArchiveFile sf
                       | otherwise        = includeFile sf
  where includeArchiveFile f = findArchives settings &&
                               isArchiveFindFile settings (findFilePath f)
        includeFile f = not (archivesOnly settings) &&
                        isFindFile settings (findFilePath f)

getFindFiles :: FindSettings -> IO [FilePath]
getFindFiles settings = do
  getRecursiveFilteredContents (startPath settings) (isFindDir settings) (isFindFile settings)

findBinaryFile :: FindSettings -> FilePath -> IO [FindResult]
findBinaryFile settings f = do
  blobEither <- getFileByteString f
  case blobEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right blob) -> return $ addFilePath (findBlob settings blob)
  where addFilePath = map (\r -> r {filePath=f})

findBlob :: FindSettings -> B.ByteString -> [FindResult]
findBlob settings blob =
  concatMap (findBlobForPattern settings blob) (findPatterns settings)

findBlobForPattern :: FindSettings -> B.ByteString -> String -> [FindResult]
findBlobForPattern settings blob = patternResults
  where lineMatchIndices :: String -> [(Int,Int)]
        lineMatchIndices p = if firstMatch settings
                               then take 1 $ matchIndices blob p
                               else matchIndices blob p
        patternResults :: String -> [FindResult]
        patternResults p = map (resultFromPatternMatchIndices p) (lineMatchIndices p)
        resultFromPatternMatchIndices :: String -> (Int, Int) -> FindResult
        resultFromPatternMatchIndices p ix =
          blankFindResult { findPattern=p
                            , lineNum=0
                            , matchStartIndex=fst ix + 1
                            , matchEndIndex=snd ix + 1
                            , line=B.empty
                            }

findTextFile :: FindSettings -> FilePath -> IO [FindResult]
findTextFile settings f =
  if multiLineFind settings
    then findTextFileContents settings f
    else findTextFileLines settings f

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
anyMatchesPattern ls p = any (`matchesPattern` p) ls

matchesAnyPattern :: B.ByteString -> [String] -> Bool
matchesAnyPattern l = any (matchesPattern l)

findTextFileContents :: FindSettings -> FilePath -> IO [FindResult]
findTextFileContents settings f = do
  contentsEither <- getFileByteString f
  case contentsEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right contents) -> return $ addFilePath (findContents settings contents)
  where addFilePath = map (\r -> r {filePath=f})

findContents :: FindSettings -> B.ByteString -> [FindResult]
findContents settings contents =
  concatMap (findContentsForPattern settings contents) (findPatterns settings)

findContentsForPattern :: FindSettings -> B.ByteString -> String -> [FindResult]
findContentsForPattern settings contents = patternResults
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
        maybeResultFromPatternMatchIndices :: String -> (Int, Int) -> Maybe FindResult
        maybeResultFromPatternMatchIndices p ix =
          if beforeLinesMatch bs && afterLinesMatch as
          then
            Just blankFindResult { findPattern=p
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

findTextFileLines :: FindSettings -> FilePath -> IO [FindResult]
findTextFileLines settings f = do
  fileLinesEither <- getFileLines f
  case fileLinesEither of
    (Left _) -> return [] -- todo: figure out to relay error
    (Right fileLines) -> return $ addFilePath (findLines settings fileLines)
  where addFilePath = map (\r -> r {filePath=f})

findLines :: FindSettings -> [B.ByteString] -> [FindResult]
findLines settings lineList = recFindLines settings [] lineList 0 []

recFindLines :: FindSettings -> [B.ByteString] -> [B.ByteString] -> Int -> [FindResult] -> [FindResult]
recFindLines settings beforeList lst num results =
  case lst of
    []     -> results
    (l:ls) -> recFindLines settings (newBefore l) ls (num + 1) (updatedResults l)
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
        newResults l = concatMap (findNextPattern l) filteredPatterns
        findNextPattern l = findLineForPattern settings (num + 1) beforeList l afterList
        filteredPatterns = if firstMatch settings
                           then filter firstMatchNotMet patterns
                           else patterns
        firstMatchNotMet p = not (any (\r -> findPattern r == p) results)
        patterns = findPatterns settings

findLineForPattern :: FindSettings -> Int -> [B.ByteString] -> B.ByteString -> [B.ByteString] -> String -> [FindResult]
findLineForPattern settings num bs l as = patternResults
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
        patternResults :: String -> [FindResult]
        patternResults p = map (resultFromPatternMatchIndices p) (lineMatchIndices p)
        resultFromPatternMatchIndices :: String -> (Int, Int) -> FindResult
        resultFromPatternMatchIndices p ix =
          blankFindResult { findPattern=p
                            , lineNum=num
                            , matchStartIndex=fst ix + 1
                            , matchEndIndex=snd ix + 1
                            , line=l
                            , beforeLines=bs
                            , afterLines=as
                            }

doFindFile :: FindSettings -> (FilePath,FileType) -> IO [FindResult]
doFindFile settings ft =
  case snd ft of
    Binary -> findBinaryFile settings $ fst ft
    filetype | filetype `elem` [Code, Text, Xml] -> findTextFile settings $ fst ft
    _ -> return []

doFindFiles :: FindSettings -> [FilePath] -> IO [FindResult]
doFindFiles settings findFiles = do
  fileTypes <- getFileTypes findFiles
  results <- mapM (doFindFile settings) (zip findFiles fileTypes)
  return $ concat results

doFind :: FindSettings -> IO [FindResult]
doFind settings = do
  findFiles <- getFindFiles settings
  doFindFiles settings findFiles
