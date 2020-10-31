module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, toUpper)
import Data.List (nub, sort, sortBy)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.TimeIt

import HsSearch.FileUtil (pathExists)
import HsSearch.SearchOptions
import HsSearch.Searcher (getSearchDirs, getSearchFiles, doSearchFiles)
import HsSearch.SearchResult
import HsSearch.SearchSettings


validateSettings :: SearchSettings -> [String]
validateSettings settings = concatMap ($settings) validators
  where validators = [ \s -> ["Startpath not defined" | startPath s == ""]
                     , \s -> ["No search patterns defined" | null (searchPatterns s)]
                     ]

errsOrUsage :: [SearchOption] -> SearchSettings -> Maybe String
errsOrUsage searchOptions settings =
  case usage of
    "" -> Nothing
    _  -> Just usage
  where errs   = validateSettings settings
        errMsg = if not (null errs)
                 then "\nERROR: " ++ head errs ++ "\n\n"
                 else ""
        usage  = case (printUsage settings, not (null errMsg)) of
                   (True, _)     -> "\n" ++ getUsage searchOptions
                   (False, True) -> errMsg ++ getUsage searchOptions
                   _ -> ""
        
formatResults :: SearchSettings -> [SearchResult] -> String
formatResults settings results =
  "\nSearch results (" ++ show (length results) ++ "):\n" ++
    (if not (null results)
       then unlines (map (formatSearchResult settings) results)
       else "")
  where matchString p = "\nMatches for "++ show p ++ ": " ++
          show (patternCount p) ++ "\n"
        patternCount :: String -> Int
        patternCount p = foldr accMatches 0 results
          where accMatches x acc = if p == searchPattern x then acc + 1 else acc

getMatchingDirs :: [SearchResult] -> [FilePath]
getMatchingDirs = sort . nub . map getDirectory
  where getDirectory r = takeDirectory (filePath r)

formatMatchingDirs :: [SearchResult] -> String
formatMatchingDirs results = 
  "\nDirectories with matches (" ++ show (length matchingDirs) ++ "):\n" ++
  unlines matchingDirs
  where matchingDirs = getMatchingDirs results

getMatchingFiles :: [SearchResult] -> [FilePath]
getMatchingFiles = sort . nub . map filePath

formatMatchingFiles :: [SearchResult] -> String
formatMatchingFiles results = 
  "\nFiles with matches (" ++ show (length matchingFiles) ++ "):\n" ++
  unlines matchingFiles
  where matchingFiles = getMatchingFiles results

byteStringToUpper :: B.ByteString -> B.ByteString
byteStringToUpper = BC.pack . map toUpper . BC.unpack

sortCaseInsensitive :: [B.ByteString] -> [B.ByteString]
sortCaseInsensitive = sortBy compareCaseInsensitive
  where compareCaseInsensitive a b = byteStringToUpper a `compare` byteStringToUpper b

getMatchingLines :: [SearchResult] -> Bool -> [B.ByteString]
getMatchingLines results unique | unique = (sortCaseInsensitive . nub . map trimLine) results
                                | otherwise = (sortCaseInsensitive . map trimLine) results
  where trimLine = BC.dropWhile isSpace . line

formatMatchingLines :: [SearchResult] -> Bool -> String
formatMatchingLines results unique = 
  "\n" ++ hdrText ++ " (" ++ show (length matchingLines) ++ "):\n" ++
  BC.unpack (BC.intercalate (BC.pack "\n") matchingLines) ++ "\n"
  where matchingLines = getMatchingLines results unique
        hdrText = if unique
                  then "Unique lines with matches"
                  else "Lines with matches"

formatSearchDirs :: [FilePath] -> String
formatSearchDirs dirs = 
  "\nDirectories to be searched (" ++ show (length dirs) ++ "):\n" ++
  unlines dirs

formatSearchFiles :: [FilePath] -> String
formatSearchFiles files = 
  "\nFiles to be searched (" ++ show (length files) ++ "):\n" ++
  unlines files

logMsg :: String -> IO ()
logMsg = putStr

main :: IO ()
main = do
  args <- getArgs
  searchOptions <- getSearchOptions
  case settingsFromArgs searchOptions args of
    Left errMsg -> logMsg $ "\nERROR: " ++ errMsg ++ "\n" ++ getUsage searchOptions ++ "\n"
    Right settings -> do
      logMsg $ if debug settings
               then "\nsettings: " ++ show settings ++ "\n"
               else ""
      case errsOrUsage searchOptions settings of
        Just usage -> logMsg $ usage ++ "\n"
        Nothing -> do
          foundPath <- pathExists (startPath settings)
          if foundPath then do
            searchDirs <- getSearchDirs settings
            logMsg $ if verbose settings
                     then formatSearchDirs searchDirs
                     else ""
            searchFiles <- getSearchFiles settings searchDirs
            logMsg $ if verbose settings
                     then formatSearchFiles searchFiles
                     else ""
            results <- doSearchFiles settings searchFiles
            logMsg $ if printResults settings
                     then formatResults settings results
                     else ""
            logMsg $ if listDirs settings
                     then formatMatchingDirs results
                     else ""
            logMsg $ if listFiles settings
                     then formatMatchingFiles results
                     else ""
            logMsg $ if listLines settings
                     then formatMatchingLines results (uniqueLines settings)
                     else ""
            logMsg ""
          else logMsg $ "\nERROR: Startpath not found\n\n" ++ getUsage searchOptions ++ "\n"