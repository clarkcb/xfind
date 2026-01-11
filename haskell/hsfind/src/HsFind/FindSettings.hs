{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module HsFind.FindSettings
  ( FindSettings(..)
  , defaultFindSettings
  , findSettingsToString
  , getSortByForName
  , needFileSizes
  , needLastMods
  , newExtensions
  , SortBy(..)
  , updateFindSettingsFromJsonValue
  ) where

import Control.Monad (mzero, unless)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V (toList)
import GHC.Generics (Generic)

import HsFind.ConsoleColor (Color(..))
import HsFind.FileTypes (FileType, getFileTypeForName, getFileTypeName)
import HsFind.FileUtil (normalizeExtension)
import HsFind.SortBy (SortBy(..), getSortByForName, sortByToString)

data FindSettings = FindSettings {
                                   archivesOnly :: Bool
                                 , colorize :: Bool
                                 , debug :: Bool
                                 , dirColor :: Color
                                 , extColor :: Color
                                 , fileColor :: Color
                                 , followSymlinks :: Bool
                                 , inArchiveExtensions :: [String]
                                 , inArchiveFilePatterns :: [String]
                                 , inDirPatterns :: [String]
                                 , inExtensions :: [String]
                                 , inFilePatterns :: [String]
                                 , inFileTypes :: [FileType]
                                 , includeArchives :: Bool
                                 , includeHidden :: Bool
                                 , maxDepth :: Integer
                                 , maxLastMod :: Maybe UTCTime
                                 , maxSize :: Integer
                                 , minDepth :: Integer
                                 , minLastMod :: Maybe UTCTime
                                 , minSize :: Integer
                                 , outArchiveExtensions :: [String]
                                 , outArchiveFilePatterns :: [String]
                                 , outDirPatterns :: [String]
                                 , outExtensions :: [String]
                                 , outFilePatterns :: [String]
                                 , outFileTypes :: [FileType]
                                 , paths :: [String]
                                 , printDirs :: Bool
                                 , printFiles :: Bool
                                 , printUsage :: Bool
                                 , printVersion :: Bool
                                 , recursive :: Bool
                                 , sortCaseInsensitive :: Bool
                                 , sortDescending :: Bool
                                 , sortResultsBy :: SortBy
                                 , verbose :: Bool
                                 } deriving (Generic, Show, Eq)

defaultFindSettings :: FindSettings
defaultFindSettings = FindSettings {
                                     archivesOnly=False
                                   , colorize=True
                                   , debug=False
                                   , dirColor=ColorCyan
                                   , extColor=ColorYellow
                                   , fileColor=ColorMagenta
                                   , followSymlinks=False
                                   , inArchiveExtensions=[]
                                   , inArchiveFilePatterns=[]
                                   , inDirPatterns=[]
                                   , inExtensions=[]
                                   , inFilePatterns=[]
                                   , inFileTypes=[]
                                   , includeArchives=False
                                   , includeHidden=False
                                   , maxDepth = -1
                                   , maxLastMod=Nothing
                                   , maxSize=0
                                   , minDepth = -1
                                   , minLastMod=Nothing
                                   , minSize=0
                                   , outArchiveExtensions=[]
                                   , outArchiveFilePatterns=[]
                                   , outDirPatterns=[]
                                   , outExtensions=[]
                                   , outFilePatterns=[]
                                   , outFileTypes=[]
                                   , paths=[]
                                   , printDirs=False
                                   , printFiles=False
                                   , printUsage=False
                                   , printVersion=False
                                   , recursive=True
                                   , sortCaseInsensitive=False
                                   , sortDescending=False
                                   , sortResultsBy=SortByFilePath
                                   , verbose=False
                                   }

newExtensions :: String -> [String]
newExtensions x | ',' `elem` x = map normalizeExtension $ removeBlank (splitOn "," x)
                | otherwise    = [normalizeExtension x]
  where removeBlank :: [String] -> [String]
        removeBlank = filter (/="")

findSettingsToString :: FindSettings -> String
findSettingsToString settings = 
  "FindSettings(" ++
  "archivesOnly=" ++ show (archivesOnly settings) ++
  ", colorize=" ++ show (colorize settings) ++
  ", debug=" ++ show (debug settings) ++
  ", followSymlinks=" ++ show (followSymlinks settings) ++
  ", inArchiveExtensions=" ++ listToString (inArchiveExtensions settings) ++
  ", inArchiveFilePatterns=" ++ listToString (inArchiveFilePatterns settings) ++
  ", inDirPatterns=" ++ listToString (inDirPatterns settings) ++
  ", inExtensions=" ++ listToString (inExtensions settings) ++
  ", inFilePatterns=" ++ listToString (inFilePatterns settings) ++
  ", inFileTypes=" ++ fileTypesToString (inFileTypes settings) ++
  ", includeArchives=" ++ show (includeArchives settings) ++
  ", includeHidden=" ++ show (includeHidden settings) ++
  ", maxDepth=" ++ show (maxDepth settings) ++
  ", maxLastMod=" ++ lastModToString (maxLastMod settings) ++
  ", maxSize=" ++ show (maxSize settings) ++
  ", minDepth=" ++ show (minDepth settings) ++
  ", minLastMod=" ++ lastModToString (minLastMod settings) ++
  ", minSize=" ++ show (minSize settings) ++
  ", outArchiveExtensions=" ++ listToString (outArchiveExtensions settings) ++
  ", outArchiveFilePatterns=" ++ listToString (outArchiveFilePatterns settings) ++
  ", outDirPatterns=" ++ listToString (outDirPatterns settings) ++
  ", outExtensions=" ++ listToString (outExtensions settings) ++
  ", outFilePatterns=" ++ listToString (outFilePatterns settings) ++
  ", outFileTypes=" ++ fileTypesToString (outFileTypes settings) ++
  ", paths=" ++ listToString (paths settings) ++
  ", printDirs=" ++ show (printDirs settings) ++
  ", printFiles=" ++ show (printFiles settings) ++
  ", printUsage=" ++ show (printUsage settings) ++
  ", printVersion=" ++ show (printVersion settings) ++
  ", recursive=" ++ show (recursive settings) ++
  ", sortBy=" ++ sortByToString (sortResultsBy settings) ++
  ", sortCaseInsensitive=" ++ show (sortCaseInsensitive settings) ++
  ", sortDescending=" ++ show (sortDescending settings) ++
  ", verbose=" ++ show (verbose settings) ++
  ")"
  where listToString lst | null lst = "[]"
                         | otherwise = "[\"" ++ intercalate "\", \"" lst ++ "\"]"
        fileTypesToString fts = "[" ++ intercalate ", " (fileTypeNames fts) ++ "]"
        fileTypeNames = Prelude.map getFileTypeName
        lastModToString :: Maybe UTCTime -> String
        lastModToString Nothing = "0"
        lastModToString (Just t) = show t

needFileSizes :: FindSettings -> Bool
needFileSizes settings = minSize settings > 0 || maxSize settings > 0 || sortResultsBy settings == SortByFileSize

needLastMods :: FindSettings -> Bool
needLastMods settings = isJust (minLastMod settings) || isJust (maxLastMod settings) || sortResultsBy settings == SortByLastMod

-- JSON parsing stuff below here
validKeys :: [Text]
validKeys = ["archivesonly", "colorize", "debug", "dircolor", "excludearchives", "excludehidden",
             "extcolor", "filecolor", "followsymlinks", "help", "in-archiveext",
             "in-archivefilepattern", "in-dirpattern", "in-ext", "in-filepattern", "in-filetype",
             "includearchives", "includehidden", "maxdepth", "maxlastmod", "maxsize", "mindepth",
             "minlastmod", "minsize", "out-archiveextension", "out-archivefilepattern",
             "out-dirpattern", "out-ext", "out-filepattern", "out-filetype", "path", "printdirs",
             "printfiles", "recursive", "sort-caseinsensitive", "sort-descending", "sort-by",
             "verbose", "version"]

instance FromJSON FindSettings where
  parseJSON = withObject "FindSettings" $ \obj -> do
    -- Check for unknown keys
    let keysInJson = HM.keys obj
        -- keys are sorted so that output is consistent across all versions
        unknownKeys = sort $ filter (`notElem` validKeys) keysInJson
    unless (null unknownKeys) $
      fail $ "Invalid option: " ++ unpack (head unknownKeys)

    -- Parse known fields
    archivesOnly' <- obj .:? "archivesonly" .!= False
    colorize' <- obj .:? "colorize" .!= True
    debug' <- obj .:? "debug" .!= False
    -- dirColor' <- obj .:? "dircolor" .!= ColorCyan
    -- extColor' <- obj .:? "extcolor" .!= ColorYellow
    -- fileColor' <- obj .:? "filecolor" .!= ColorMagenta
    followSymlinks' <- obj .:? "followsymlinks" .!= False
    inArchiveExtensions' <- obj .:? "in-archiveext" >>= parseStringOrArray
    inArchiveFilePatterns' <- obj .:? "in-archivefilepattern" >>= parseStringOrArray
    inDirPatterns' <- obj .:? "in-dirpattern" >>= parseStringOrArray
    inExtensions' <- obj .:? "in-ext" >>= parseStringOrArray
    inFilePatterns' <- obj .:? "in-filepattern" >>= parseStringOrArray
    inFileTypes' <- obj .:? "in-filetype" >>= parseFileTypes
    includeArchives' <- parseIncludeArchives obj
    includeHidden' <- parseIncludeHidden obj
    maxDepth' <- obj .:? "maxdepth" .!= (-1)
    maxLastMod' <- obj .:? "maxlastmod" >>= parseUTCTime
    maxSize' <- obj .:? "maxsize" .!= 0
    minDepth' <- obj .:? "mindepth" .!= (-1)
    minLastMod' <- obj .:? "minlastmod" >>= parseUTCTime
    minSize' <- obj .:? "minsize" .!= 0
    outArchiveExtensions' <- obj .:? "out-archiveextension" >>= parseStringOrArray
    outArchiveFilePatterns' <- obj .:? "out-archivefilepattern" >>= parseStringOrArray
    outDirPatterns' <- obj .:? "out-dirpattern" >>= parseStringOrArray
    outExtensions' <- obj .:? "out-ext" >>= parseStringOrArray
    outFilePatterns' <- obj .:? "out-filepattern" >>= parseStringOrArray
    outFileTypes' <- obj .:? "out-filetype" >>= parseFileTypes
    paths' <- obj .:? "path" >>= parseStringOrArray
    printDirs' <- obj .:? "printdirs" .!= False
    printFiles' <- obj .:? "printfiles" .!= False
    printUsage' <- obj .:? "help" .!= False
    printVersion' <- obj .:? "version" .!= False
    recursive' <- obj .:? "recursive" .!= True
    sortCaseInsensitive' <- obj .:? "sort-caseinsensitive" .!= False
    sortDescending' <- obj .:? "sort-descending" .!= False
    sortResultsBy' <- obj .:? "sort-by" >>= parseSortBy
    verbose' <- obj .:? "verbose" .!= False
    return FindSettings {
      archivesOnly=archivesOnly'
    , colorize=colorize'
    , debug=debug'
    , dirColor=ColorCyan
    , extColor=ColorYellow
    , fileColor=ColorMagenta
    , followSymlinks=followSymlinks'
    , inArchiveExtensions=inArchiveExtensions'
    , inArchiveFilePatterns=inArchiveFilePatterns'
    , inDirPatterns=inDirPatterns'
    , inExtensions=inExtensions'
    , inFilePatterns=inFilePatterns'
    , inFileTypes=inFileTypes'
    , includeArchives=includeArchives'
    , includeHidden=includeHidden'
    , maxDepth=maxDepth'
    , maxLastMod=maxLastMod'
    , maxSize=maxSize'
    , minDepth=minDepth'
    , minLastMod=minLastMod'
    , minSize=minSize'
    , outArchiveExtensions=outArchiveExtensions'
    , outArchiveFilePatterns=outArchiveFilePatterns'
    , outDirPatterns=outDirPatterns'
    , outExtensions=outExtensions'
    , outFilePatterns=outFilePatterns'
    , outFileTypes=outFileTypes'
    , paths=paths'
    , printDirs=printDirs'
    , printFiles=printFiles'
    , printUsage=printUsage'
    , printVersion=printVersion'
    , recursive=recursive'
    , sortCaseInsensitive=sortCaseInsensitive'
    , sortDescending=sortDescending'
    , sortResultsBy=sortResultsBy'
    , verbose=verbose'
    }

-- Custom function to handle "includearchives" or "excludearchives"
parseIncludeArchives :: Object -> Parser Bool
parseIncludeArchives obj = do
  maybeIncludeArchives <- obj .:? "includearchives"
  maybeExcludeArchives <- obj .:? "excludearchives"
  case (maybeIncludeArchives, maybeExcludeArchives) of
    (Just val, _) -> return val             -- Use "includearchives" directly if present
    (Nothing, Just val) -> return (not val) -- Negate "excludearchives" if present
    -- (Nothing, Nothing) -> fail "Missing required field: either 'includearchives' or 'excludearchives'"
    (Nothing, Nothing) -> return False

-- Custom function to handle "includehidden" or "excludehidden"
parseIncludeHidden :: Object -> Parser Bool
parseIncludeHidden obj = do
  maybeIncludeHidden <- obj .:? "includehidden"
  maybeExcludeHidden <- obj .:? "excludehidden"
  case (maybeIncludeHidden, maybeExcludeHidden) of
    (Just val, _) -> return val             -- Use "includehidden" directly if present
    (Nothing, Just val) -> return (not val) -- Negate "excludehidden" if present
    -- (Nothing, Nothing) -> fail "Missing required field: either 'includehidden' or 'excludehidden'"
    (Nothing, Nothing) -> return False

-- Helper function to handle string or array of strings
parseStringOrArray :: Maybe Value -> Parser [String]
parseStringOrArray Nothing = return []
parseStringOrArray (Just v) =
  case v of
    String s -> return [unpack s]
    Array a  -> mapM (fmap unpack . parseJSON) (V.toList a)
    _        -> mzero

-- Helper function to parse FileType
parseFileTypes :: Maybe Value -> Parser [FileType]
parseFileTypes Nothing = return []
parseFileTypes (Just v) =
  case v of
    String s -> return [getFileTypeForName (unpack s)]
    Array a  -> mapM (fmap (getFileTypeForName . unpack) . parseJSON) (V.toList a)
    _        -> mzero

-- Helper function to parse optional UTCTime
parseUTCTime :: Maybe Value -> Parser (Maybe UTCTime)
parseUTCTime Nothing = return Nothing
parseUTCTime (Just (String s)) =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (unpack s) of
    Just time -> return (Just time)
    Nothing   -> fail "Invalid UTCTime format"
parseUTCTime _ = mzero

-- -- Helper function to parse Color
-- parseColor :: Maybe Value -> Parser Color
-- parseColor Nothing = return ColorBlack
-- parseColor (Just (String s)) = return $ getColorForName (unpack s)
-- parseColor _ = mzero

-- Helper function to parse SortBy
parseSortBy :: Maybe Value -> Parser SortBy
parseSortBy Nothing = return SortByFilePath
parseSortBy (Just (String s)) = return $ getSortByForName (unpack s)
parseSortBy _ = mzero

updateFindSettingsFromJsonValue :: FindSettings -> Value -> Either String FindSettings
updateFindSettingsFromJsonValue settings jsonVal =
  case fromJSON jsonVal of
    Success newSettings -> Right $ mergeFindSettings settings newSettings
    Error e             -> Left e

mergeFindSettings :: FindSettings -> FindSettings -> FindSettings
mergeFindSettings old new = FindSettings
  { archivesOnly = archivesOnly new || archivesOnly old
  , colorize = colorize new || colorize old
  , debug = debug new || debug old
  -- , dirColor = dirColor new || dirColor old
  -- , extColor = extColor new || extColor old
  -- , fileColor = fileColor new || fileColor old
  , followSymlinks = followSymlinks new || followSymlinks old
  , inArchiveExtensions = inArchiveExtensions old ++ inArchiveExtensions new
  , inArchiveFilePatterns = inArchiveFilePatterns old ++ inArchiveFilePatterns new
  , inDirPatterns = inDirPatterns old ++ inDirPatterns new
  , inExtensions = inExtensions old ++ inExtensions new
  , inFilePatterns = inFilePatterns old ++ inFilePatterns new
  , inFileTypes = inFileTypes old ++ inFileTypes new
  , includeArchives = includeArchives new || includeArchives old
  , includeHidden = includeHidden new || includeHidden old
  , maxDepth = if maxDepth new > -1 then maxDepth new else maxDepth old
  , maxLastMod = if isJust (maxLastMod new) then maxLastMod new else maxLastMod old
  , maxSize = if maxSize new > 0 then maxSize new else maxSize old
  , minDepth = if minDepth new > -1 then minDepth new else minDepth old
  , minLastMod = if isJust (minLastMod new) then minLastMod new else minLastMod old
  , minSize = if minSize new > 0 then minSize new else minSize old
  , outArchiveExtensions = outArchiveExtensions old ++ outArchiveExtensions new
  , outArchiveFilePatterns = outArchiveFilePatterns old ++ outArchiveFilePatterns new
  , outDirPatterns = outDirPatterns old ++ outDirPatterns new
  , outExtensions = outExtensions old ++ outExtensions new
  , outFilePatterns = outFilePatterns old ++ outFilePatterns new
  , outFileTypes = outFileTypes old ++ outFileTypes new
  , paths = paths old ++ paths new
  , printDirs = printDirs new || printDirs old
  , printFiles = printFiles new || printFiles old
  , printUsage = printUsage new || printUsage old
  , printVersion = printVersion new || printVersion old
    -- recursive is true by default, so a false value overrides
  , recursive = if not (recursive new) then recursive new else recursive old
  , sortCaseInsensitive = sortCaseInsensitive new || sortCaseInsensitive old
  , sortDescending = sortDescending new || sortDescending old
  , sortResultsBy = if sortResultsBy new /= SortByFilePath then sortResultsBy new else sortResultsBy old
  , verbose = verbose new || verbose old
  }
