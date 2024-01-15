{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction #-}
module HsFind.FindOptions (
    FindOption(..)
  , getFindOptions
  , getUsage
  , parseDateToUtc
  , settingsFromArgs) where

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char (toLower)
import Data.Either (isLeft, lefts, rights)
import Data.List (isPrefixOf, sortBy)
import Data.Maybe (isJust)

import GHC.Generics
import Data.Aeson

import HsFind.Paths_hsfind (getDataFileName)
import HsFind.FileTypes (getFileTypeForName)
import HsFind.FileUtil (getFileString)
import HsFind.FindSettings
import Data.Time (UTCTime(..), parseTimeM, defaultTimeLocale)

data FindOption = FindOption
  { long :: String
  , short :: Maybe String
  , desc :: String
  } deriving (Show, Eq, Generic)

instance FromJSON FindOption

newtype FindOptions
  = FindOptions {findoptions :: [FindOption]}
  deriving (Show, Eq, Generic)

instance FromJSON FindOptions

findOptionsFile :: FilePath
findOptionsFile = "findoptions.json"

getFindOptions :: IO [FindOption]
getFindOptions = do
  findOptionsPath <- getDataFileName findOptionsFile
  findOptionsJsonString <- getFileString findOptionsPath
  case findOptionsJsonString of
    (Left _) -> return []
    (Right jsonString) ->
      case (eitherDecode (BC.pack jsonString) :: Either String FindOptions) of
        (Left e) -> return [FindOption {long=e, short=Nothing, desc=e}]
        (Right jsonFindOptions) -> return (findoptions jsonFindOptions)

getUsage :: [FindOption] -> String
getUsage findOptions =
  "Usage:\n hsfind [options] <path> [<path> ...]\n\nOptions:\n" ++
  findOptionsToString findOptions

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

data ActionType = ArgActionType
                | BoolFlagActionType
                | UnknownActionType
  deriving (Show, Eq)

type ArgAction = FindSettings -> String -> FindSettings
type BoolFlagAction = FindSettings -> Bool -> FindSettings

argActions :: [(String, ArgAction)]
argActions = [ ("in-archiveext", \ss s -> ss {inArchiveExtensions = inArchiveExtensions ss ++ newExtensions s})
             , ("in-archivefilepattern", \ss s -> ss {inArchiveFilePatterns = inArchiveFilePatterns ss ++ [s]})
             , ("in-dirpattern", \ss s -> ss {inDirPatterns = inDirPatterns ss ++ [s]})
             , ("in-ext", \ss s -> ss {inExtensions = inExtensions ss ++ newExtensions s})
             , ("in-filepattern", \ss s -> ss {inFilePatterns = inFilePatterns ss ++ [s]})
             , ("in-filetype", \ss s -> ss {inFileTypes = inFileTypes ss ++ [getFileTypeForName s]})
             , ("maxdepth", \ss s -> ss {maxDepth = read s})
             , ("maxlastmod", \ss s -> ss {maxLastMod = parseDateToUtc s})
             , ("maxsize", \ss s -> ss {maxSize = read s})
             , ("mindepth", \ss s -> ss {minDepth = read s})
             , ("minlastmod", \ss s -> ss {minLastMod = parseDateToUtc s})
             , ("minsize", \ss s -> ss {minSize = read s})
             , ("out-archiveext", \ss s -> ss {outArchiveExtensions = outArchiveExtensions ss ++ newExtensions s})
             , ("out-archivefilepattern", \ss s -> ss {outArchiveFilePatterns = outArchiveFilePatterns ss ++ [s]})
             , ("out-dirpattern", \ss s -> ss {outDirPatterns = outDirPatterns ss ++ [s]})
             , ("out-ext", \ss s -> ss {outExtensions = outExtensions ss ++ newExtensions s})
             , ("out-filepattern", \ss s -> ss {outFilePatterns = outFilePatterns ss ++ [s]})
             , ("out-filetype", \ss s -> ss {outFileTypes = outFileTypes ss ++ [getFileTypeForName s]})
             , ("path", \ss s -> ss {paths = paths ss ++ [s]})
             , ("sort-by", \ss s -> ss {sortResultsBy = getSortByForName s})
             ]

boolFlagActions :: [(String, BoolFlagAction)]
boolFlagActions = [ ("archivesonly", \ss b -> ss {archivesOnly=b,
                                                  includeArchives=b})
                  , ("debug", \ss b -> ss {debug=b, verbose=b})
                  , ("excludearchives", \ss b -> ss {includeArchives=not b})
                  , ("excludehidden", \ss b -> ss {includeHidden=not b})
                  , ("help", \ss b -> ss {printUsage=b})
                  , ("includearchives", \ss b -> ss {includeArchives=b})
                  , ("includehidden", \ss b -> ss {includeHidden=b})
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

shortToLong :: [FindOption] -> String -> Either String String
shortToLong _ "" = Left "Missing argument"
shortToLong opts s | length s == 2 && head s == '-' =
                      if any (\so -> short so == Just (tail s)) optsWithShort
                      then Right $ "--" ++ getLongForShort s
                      else Left $ "Invalid option: " ++ tail s ++ "\n"
                   | otherwise = Right s
  where optsWithShort = filter (isJust . short) opts
        getLongForShort x = (long . head . filter (\so -> short so == Just (tail x))) optsWithShort

settingsFromArgs :: [FindOption] -> [String] -> Either String FindSettings
settingsFromArgs opts arguments =
  if any isLeft longArgs
  then (Left . head . lefts) longArgs
  else
    -- default printFiles to true since running as cli
    recSettingsFromArgs defaultFindSettings{printFiles=True} $ rights longArgs
  where recSettingsFromArgs :: FindSettings -> [String] -> Either String FindSettings
        recSettingsFromArgs settings args =
          case args of
          [] -> Right settings
          [a] | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              ArgActionType -> Left $ "Missing value for option: " ++ a ++ "\n"
              BoolFlagActionType -> recSettingsFromArgs (getBoolFlagAction (argName a) settings True) []
              UnknownActionType -> Left $ "Invalid option: " ++ a ++ "\n"
          a:as | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              ArgActionType -> recSettingsFromArgs (getArgAction (argName a) settings (head as)) (tail as)
              BoolFlagActionType -> recSettingsFromArgs (getBoolFlagAction (argName a) settings True) as
              UnknownActionType -> Left $ "Invalid option: " ++ argName a ++ "\n"
          a:as -> recSettingsFromArgs (settings {paths = paths settings ++ [a]}) as
        longArgs :: [Either String String]
        longArgs = map (shortToLong opts) arguments
        getActionType :: String -> ActionType
        getActionType a
          | isArgAction a = ArgActionType
          | isBoolFlagAction a = BoolFlagActionType
          | otherwise = UnknownActionType
        argName :: String -> String
        argName = dropWhile (=='-')
        getArgAction :: String -> ArgAction
        getArgAction a = snd $ head $ filter (\(x,_) -> a==x) argActions
        getBoolFlagAction :: String -> BoolFlagAction
        getBoolFlagAction a = snd $ head $ filter (\(x,_) -> a==x) boolFlagActions
        isArgAction :: String -> Bool
        isArgAction a = isJust $ lookup a argActions
        isBoolFlagAction :: String -> Bool
        isBoolFlagAction a = isJust $ lookup a boolFlagActions
