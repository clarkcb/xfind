module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, toUpper)
import Data.List (nub, sort, sortBy)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)

import HsFind.FileUtil (getParentPath, pathExists)
import HsFind.FindOptions
import HsFind.Finder (getFindFiles, doFindFiles)
import HsFind.FindResult
import HsFind.FindSettings


validateSettings :: FindSettings -> [String]
validateSettings settings = concatMap ($settings) validators
  where validators = [ \s -> ["Startpath not defined" | startPath s == ""]
                     , \s -> ["No find patterns defined" | null (findPatterns s)]
                     , \s -> ["Invalid lines after" | linesAfter s < 0]
                     , \s -> ["Invalid lines before" | linesBefore s < 0]
                     , \s -> ["Invalid max line length" | maxLineLength s < 0]
                     ]

errsOrUsage :: [FindOption] -> FindSettings -> Maybe String
errsOrUsage findOptions settings =
  case usage of
    "" -> Nothing
    _  -> Just usage
  where errs   = validateSettings settings
        errMsg = if not (null errs)
                 then "\nERROR: " ++ head errs ++ "\n\n"
                 else ""
        usage  = case (printUsage settings, not (null errMsg)) of
                   (True, _)     -> "\n" ++ getUsage findOptions
                   (False, True) -> errMsg ++ getUsage findOptions
                   _ -> ""

formatResults :: FindSettings -> [FindResult] -> String
formatResults settings results =
  "\nFind results (" ++ show (length results) ++ "):\n" ++
    (if not (null results)
       then unlines (map (formatFindResult settings) results)
       else "")

getMatchingDirs :: [FindResult] -> [FilePath]
getMatchingDirs = sort . nub . map getDirectory
  where getDirectory r = takeDirectory (filePath r)

formatMatchingDirs :: [FindResult] -> String
formatMatchingDirs results = 
  "\nDirectories with matches (" ++ show (length matchingDirs) ++ "):\n" ++
  unlines matchingDirs
  where matchingDirs = getMatchingDirs results

getMatchingFiles :: [FindResult] -> [FilePath]
getMatchingFiles = sort . nub . map filePath

formatMatchingFiles :: [FindResult] -> String
formatMatchingFiles results = 
  "\nFiles with matches (" ++ show (length matchingFiles) ++ "):\n" ++
  unlines matchingFiles
  where matchingFiles = getMatchingFiles results

byteStringToUpper :: B.ByteString -> B.ByteString
byteStringToUpper = BC.pack . map toUpper . BC.unpack

sortCaseInsensitive :: [B.ByteString] -> [B.ByteString]
sortCaseInsensitive = sortBy compareCaseInsensitive
  where compareCaseInsensitive a b = byteStringToUpper a `compare` byteStringToUpper b

getMatchingLines :: [FindResult] -> Bool -> [B.ByteString]
getMatchingLines results unique | unique = (sortCaseInsensitive . nub . map trimLine) results
                                | otherwise = (sortCaseInsensitive . map trimLine) results
  where trimLine = BC.dropWhile isSpace . line

formatMatchingLines :: [FindResult] -> Bool -> String
formatMatchingLines results unique = 
  "\n" ++ hdrText ++ " (" ++ show (length matchingLines) ++ "):\n" ++
  BC.unpack (BC.intercalate (BC.pack "\n") matchingLines) ++ "\n"
  where matchingLines = getMatchingLines results unique
        hdrText = if unique
                  then "Unique lines with matches"
                  else "Lines with matches"

formatFindDirs :: [FilePath] -> String
formatFindDirs dirs = 
  "\nDirectories to be found (" ++ show (length dirs) ++ "):\n" ++
  unlines (sort dirs)

formatFindFiles :: [FilePath] -> String
formatFindFiles files =
  formatFindDirs (nub (map getParentPath files)) ++
  "\nFiles to be found (" ++ show (length files) ++ "):\n" ++
  unlines (sort files)

logMsg :: String -> IO ()
logMsg = putStr

main :: IO ()
main = do
  args <- getArgs
  findOptions <- getFindOptions
  case settingsFromArgs findOptions args of
    Left errMsg -> logMsg $ "\nERROR: " ++ errMsg ++ "\n" ++ getUsage findOptions ++ "\n"
    Right settings -> do
      logMsg $ if debug settings
               then "\nsettings: " ++ show settings ++ "\n"
               else ""
      case errsOrUsage findOptions settings of
        Just usage -> logMsg $ usage ++ "\n"
        Nothing -> do
          foundPath <- pathExists (startPath settings)
          if foundPath then do
            findFiles <- getFindFiles settings
            logMsg $ if verbose settings
                     then formatFindFiles findFiles
                     else ""
            results <- doFindFiles settings findFiles
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
          else logMsg $ "\nERROR: Startpath not found\n\n" ++ getUsage findOptions ++ "\n"
