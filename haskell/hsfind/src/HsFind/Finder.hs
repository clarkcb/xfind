module HsFind.Finder
    (
      doFind
    , filterFile
    , getFindFiles
    , isArchiveFindFile
    , isFindDir
    , isFindFile
    ) where

import Control.Monad (forM)
import Data.Maybe (fromJust, isJust)
import qualified Data.ByteString as B
import Text.Regex.PCRE

import HsFind.FileTypes
import HsFind.FileUtil
import HsFind.FindFile
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
filterFile settings ff | isArchiveFile ff = includeArchiveFile ff
                       | otherwise        = includeFile ff
  where includeArchiveFile f = includeArchives settings &&
                               isArchiveFindFile settings (findFilePath f)
        includeFile f = not (archivesOnly settings) &&
                        isFindFile settings (findFilePath f)

filterToFindFile :: FindSettings -> (FilePath,FileType) -> Maybe FindFile
filterToFindFile settings ft =
  if (null inTypes || snd ft `elem` inTypes) && (null outTypes || notElem (snd ft) outTypes)
  then Just blankFindFile { findFilePath=fst ft
                          , findFileType=snd ft
                          }
  else Nothing
  where inTypes = inFileTypes settings
        outTypes = outFileTypes settings

getFindFiles :: FindSettings -> IO [FindFile]
getFindFiles settings = do
  paths <- forM (paths settings) $ \path ->
    getRecursiveFilteredContents path (isFindDir settings) (isFindFile settings)
  let allPaths = concat paths
  allFileTypes <- getFileTypes allPaths
  let justFindFiles = filter isJust (map (filterToFindFile settings) (zip allPaths allFileTypes))
  return $ map fromJust justFindFiles

doFind :: FindSettings -> IO [FindFile]
doFind = getFindFiles
