{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction #-}
module HsFind.FindOptions (
    FindOption(..)
  , FindOptions(..)
  , getFindOptions
  , getUsage
  , parseDateToUtc
  , settingsFromArgs
  , settingsFromFile
  , settingsFromJson) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char (toLower)
import Data.Either (isLeft, lefts, rights, either)
import Data.List (isPrefixOf, isSuffixOf, sortBy)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Time (UTCTime(..), parseTimeM, defaultTimeLocale)
import GHC.Generics

import HsFind.Paths_hsfind (getDataFileName)
import HsFind.ArgTokenizer
import HsFind.FileTypes (getFileTypeForName)
import HsFind.FileUtil (expandPath, getFileString, isFile)
import HsFind.FindSettings


data FindOption = FindOption
  { long :: String
  , short :: Maybe String
  , desc :: String
  } deriving (Show, Eq, Generic)

instance FromJSON FindOption

newtype JsonFindOptions
  = JsonFindOptions {findoptions :: [FindOption]}
  deriving (Show, Eq, Generic)

instance FromJSON JsonFindOptions

data FindOptions = FindOptions
  { options :: [FindOption]
  , argTokenizer :: ArgTokenizer
  } deriving (Show, Eq, Generic)

findOptionsFile :: FilePath
findOptionsFile = "findoptions.json"

getArgTokenizer :: [FindOption] -> ArgTokenizer
getArgTokenizer jsonOpts =
  ArgTokenizer
    { boolMap = boolMap
    , stringMap = stringMap
    , intMap = intMap
    }
  where
    boolMap = boolShortMap ++ boolLongMap
    stringMap = stringShortMap ++ stringLongMap ++ [("path", "path"), ("settings-file", "settings-file")]
    intMap = intShortMap ++ intLongMap
    boolLongMap :: [(String, String)]
    boolLongMap = [(long o, long o) | o <- jsonOpts, isBoolOption o]
    boolShortMap :: [(String, String)]
    boolShortMap = [(fromJust (short o), long o) | o <- jsonOpts, isBoolOption o, isJust (short o)]
    stringLongMap :: [(String, String)]
    stringLongMap = [(long o, long o) | o <- jsonOpts, isStringOption o]
    stringShortMap :: [(String, String)]
    stringShortMap = [(fromJust (short o), long o) | o <- jsonOpts, isStringOption o, isJust (short o)]
    intLongMap :: [(String, String)]
    intLongMap = [(long o, long o) | o <- jsonOpts, isIntOption o]
    intShortMap :: [(String, String)]
    intShortMap = [(fromJust (short o), long o) | o <- jsonOpts, isIntOption o, isJust (short o)]
    isBoolOption :: FindOption -> Bool
    isBoolOption o = long o `elem` map fst boolActions
    isStringOption :: FindOption -> Bool
    isStringOption o = long o `elem` map fst stringActions
    isIntOption :: FindOption -> Bool
    isIntOption o = long o `elem` map fst integerActions

getFindOptions :: IO (Either String FindOptions)
getFindOptions = do
  findOptionsPath <- getDataFileName findOptionsFile
  findOptionsJsonString <- getFileString findOptionsPath
  case findOptionsJsonString of
    Left e -> return $ Left e
    Right jsonString ->
      case (eitherDecode (BC.pack jsonString) :: Either String JsonFindOptions) of
        Left e -> return $ Left e
        Right jsonFindOptions -> return $ Right FindOptions { options = findoptions jsonFindOptions,
                                                              argTokenizer = getArgTokenizer (findoptions jsonFindOptions) }

getUsage :: FindOptions -> String
getUsage findOptions =
  "Usage:\n hsfind [options] <path> [<path> ...]\n\nOptions:\n" ++ findOptionsToString (options findOptions)

getOptStrings :: [FindOption] -> [String]
getOptStrings = map formatOpts
  where formatOpts FindOption {long=l, short=Nothing} = getLong l
        formatOpts FindOption {long=l, short=Just s}  = shortAndLong s l
        getLong l = "--" ++ l
        shortAndLong s l = "-" ++ s ++ "," ++ getLong l

getOptDesc :: FindOption -> String
getOptDesc FindOption {desc=""} = error "No description for FindOption"
getOptDesc FindOption {desc=d} = d

sortFindOption :: FindOption -> FindOption -> Ordering
sortFindOption FindOption {long=l1, short=s1} FindOption {long=l2, short=s2} =
  compare (shortOrLong s1 l1) (shortOrLong s2 l2)
  where
    shortOrLong Nothing l = l
    shortOrLong (Just s) l = map toLower s ++ "@" ++ l

sortFindOptions :: [FindOption] -> [FindOption]
sortFindOptions = sortBy sortFindOption

padString :: String -> Int -> String
padString s len | length s < len = s ++ replicate (len - length s) ' '
                | otherwise      = s

findOptionsToString :: [FindOption] -> String
findOptionsToString findOptions = 
  unlines $ zipWith formatOptLine optStrings optDescs
  where
    sorted = sortFindOptions findOptions
    optStrings = getOptStrings sorted
    optDescs = map getOptDesc sorted
    longest = maximum $ map length optStrings
    formatOptLine o d = " " ++ padString o longest ++ "  " ++ d

parseDateToUtc :: String -> Maybe UTCTime
parseDateToUtc dateString = 
  parseTimeM True defaultTimeLocale "%Y-%-m-%-d" dateString :: Maybe UTCTime

data ActionType = BoolActionType
                | StringActionType
                | IntegerActionType
                | UnknownActionType
  deriving (Show, Eq)

type BoolAction = FindSettings -> Bool -> FindSettings
type StringAction = FindSettings -> String -> FindSettings
type IntegerAction = FindSettings -> Integer -> FindSettings

boolActions :: [(String, BoolAction)]
boolActions = [ ("archivesonly", \ss b -> ss {archivesOnly=b, includeArchives=b})
              , ("colorize", \ss b -> ss {colorize=b})
              , ("debug", \ss b -> ss {debug=b, verbose=b})
              , ("excludearchives", \ss b -> ss {includeArchives=not b})
              , ("excludehidden", \ss b -> ss {includeHidden=not b})
              , ("followsymlinks", \ss b -> ss {followSymlinks=b})
              , ("help", \ss b -> ss {printUsage=b})
              , ("includearchives", \ss b -> ss {includeArchives=b})
              , ("includehidden", \ss b -> ss {includeHidden=b})
              , ("nocolorize", \ss b -> ss {colorize=not b})
              , ("nofollowsymlinks", \ss b -> ss {followSymlinks=not b})
              , ("noprintdirs", \ss b -> ss {printDirs=b})
              , ("noprintfiles", \ss b -> ss {printFiles=not b})
              , ("norecursive", \ss b -> ss {recursive=not b})
              , ("printdirs", \ss b -> ss {printDirs=b})
              , ("printfiles", \ss b -> ss {printFiles=b})
              , ("recursive", \ss b -> ss {recursive=b})
              , ("sort-ascending", \ss b -> ss {sortDescending=not b})
              , ("sort-caseinsensitive", \ss b -> ss {sortCaseInsensitive=b})
              , ("sort-casesensitive", \ss b -> ss {sortCaseInsensitive=not b})
              , ("sort-descending", \ss b -> ss {sortDescending=b})
              , ("verbose", \ss b -> ss {verbose=b})
              , ("version", \ss b -> ss {printVersion=b})
              ]

stringActions :: [(String, StringAction)]
stringActions = [ ("in-archiveext", \ss s -> ss {inArchiveExtensions = inArchiveExtensions ss ++ newExtensions s})
                , ("in-archivefilepattern", \ss s -> ss {inArchiveFilePatterns = inArchiveFilePatterns ss ++ [s]})
                , ("in-dirpattern", \ss s -> ss {inDirPatterns = inDirPatterns ss ++ [s]})
                , ("in-ext", \ss s -> ss {inExtensions = inExtensions ss ++ newExtensions s})
                , ("in-filepattern", \ss s -> ss {inFilePatterns = inFilePatterns ss ++ [s]})
                , ("in-filetype", \ss s -> ss {inFileTypes = inFileTypes ss ++ [getFileTypeForName s]})
                , ("maxlastmod", \ss s -> ss {maxLastMod = parseDateToUtc s})
                , ("minlastmod", \ss s -> ss {minLastMod = parseDateToUtc s})
                , ("out-archiveext", \ss s -> ss {outArchiveExtensions = outArchiveExtensions ss ++ newExtensions s})
                , ("out-archivefilepattern", \ss s -> ss {outArchiveFilePatterns = outArchiveFilePatterns ss ++ [s]})
                , ("out-dirpattern", \ss s -> ss {outDirPatterns = outDirPatterns ss ++ [s]})
                , ("out-ext", \ss s -> ss {outExtensions = outExtensions ss ++ newExtensions s})
                , ("out-filepattern", \ss s -> ss {outFilePatterns = outFilePatterns ss ++ [s]})
                , ("out-filetype", \ss s -> ss {outFileTypes = outFileTypes ss ++ [getFileTypeForName s]})
                , ("path", \ss s -> ss {paths = paths ss ++ [s]})
                , ("sort-by", \ss s -> ss {sortResultsBy = getSortByForName s})
                ]

integerActions :: [(String, IntegerAction)]
integerActions = [ ("maxdepth", \ss i -> ss {maxDepth = i})
                 , ("maxsize", \ss i -> ss {maxSize = i})
                 , ("mindepth", \ss i -> ss {minDepth = i})
                 , ("minsize", \ss i -> ss {minSize = i})
                 ]

updateSettingsFromTokens :: FindSettings -> FindOptions -> [ArgToken] -> IO (Either String FindSettings)
updateSettingsFromTokens settings findOptions tokens = do
  case tokens of
    [] -> return $ Right settings
    t:ts ->
      case getActionType (name t) of
        BoolActionType ->
          case value t of
            TypeA b -> updateSettingsFromTokens (getBoolAction (name t) settings b) findOptions ts
            _ -> return $ Left $ "Invalid boolean value for option: " ++ name t
        StringActionType ->
          case value t of
            TypeB s ->
              if name t == "settings-file"
              then do
                settingsEither <- updateSettingsFromFile settings findOptions s
                case settingsEither of
                  Left e -> return $ Left e
                  Right settings' -> updateSettingsFromTokens settings' findOptions ts
              else updateSettingsFromTokens (getStringAction (name t) settings s) findOptions ts
            _ -> return $ Left $ "Invalid string value for option: " ++ name t
        IntegerActionType ->
          case value t of
            TypeC i -> updateSettingsFromTokens (getIntegerAction (name t) settings (toInteger i)) findOptions ts
            _ -> return $ Left $ "Invalid integer value for option: " ++ name t
        UnknownActionType -> return $ Left $ "Invalid option from updateSettingsFromTokens: " ++ name t
  where
    getActionType :: String -> ActionType
    getActionType a
      | isBoolAction a = BoolActionType
      | isStringAction a = StringActionType
      | a == "settings-file" = StringActionType
      | isIntegerAction a = IntegerActionType
      | otherwise = UnknownActionType
    getBoolAction :: String -> BoolAction
    getBoolAction a = snd $ head $ filter (\(x,_) -> a==x) boolActions
    getStringAction :: String -> StringAction
    getStringAction a = snd $ head $ filter (\(x,_) -> a==x) stringActions
    getIntegerAction :: String -> IntegerAction
    getIntegerAction a = snd $ head $ filter (\(x,_) -> a==x) integerActions
    isBoolAction :: String -> Bool
    isBoolAction a = isJust $ lookup a boolActions
    isStringAction :: String -> Bool
    isStringAction a = isJust $ lookup a stringActions
    isIntegerAction :: String -> Bool
    isIntegerAction a = isJust $ lookup a integerActions

updateSettingsFromJson :: FindSettings -> FindOptions -> String -> IO (Either String FindSettings)
updateSettingsFromJson settings findOptions jsonStr = do
  let eitherTokens = tokenizeJson (argTokenizer findOptions) jsonStr
  case eitherTokens of
    Left e -> return $ Left e
    Right tokens -> updateSettingsFromTokens settings findOptions tokens

settingsFromJson :: FindOptions -> String -> IO (Either String FindSettings)
settingsFromJson = updateSettingsFromJson defaultFindSettings

updateSettingsFromFile :: FindSettings -> FindOptions -> FilePath -> IO (Either String FindSettings)
updateSettingsFromFile settings findOptions filePath = do
  eitherTokens <- tokenizeFile (argTokenizer findOptions) filePath
  case eitherTokens of
    Left e -> return $ Left e
    Right tokens -> updateSettingsFromTokens settings findOptions tokens

settingsFromFile :: FindOptions -> FilePath -> IO (Either String FindSettings)
settingsFromFile = updateSettingsFromFile defaultFindSettings

updateSettingsFromArgs :: FindSettings -> FindOptions -> [String] -> IO (Either String FindSettings)
updateSettingsFromArgs settings findOptions arguments = do
  case tokenizeArgs (argTokenizer findOptions) arguments of
    Left e -> return $ Left e
    Right tokens -> updateSettingsFromTokens settings findOptions tokens

settingsFromArgs :: FindOptions -> [String] -> IO (Either String FindSettings)
settingsFromArgs = updateSettingsFromArgs defaultFindSettings{printFiles=True}
