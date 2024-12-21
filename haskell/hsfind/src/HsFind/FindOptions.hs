{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction #-}
module HsFind.FindOptions (
    FindOption(..)
  , getFindOptions
  , getUsage
  , parseDateToUtc
  , ioSettingsFromArgs
  , settingsFromArgs) where

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char (toLower)
import Data.Either (isLeft, lefts, rights, Either (Left))
import Data.List (isPrefixOf, isSuffixOf, sortBy)
import Data.Maybe (isJust)

import GHC.Generics
import Data.Aeson

import HsFind.Paths_hsfind (getDataFileName)
import HsFind.FileTypes (getFileTypeForName)
import HsFind.FileUtil (getFileString, isFile)
import HsFind.FindSettings
import Data.Time (UTCTime(..), parseTimeM, defaultTimeLocale)
import Data.ByteString (putStr)

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

getFindOptions :: IO (Either String [FindOption])
getFindOptions = do
  findOptionsPath <- getDataFileName findOptionsFile
  findOptionsJsonString <- getFileString findOptionsPath
  case findOptionsJsonString of
    Left e -> return $ Left e
    Right jsonString ->
      case (eitherDecode (BC.pack jsonString) :: Either String FindOptions) of
        Left e -> return $ Left e
        Right jsonFindOptions -> return $ Right (findoptions jsonFindOptions)

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
              , ("debug", \ss b -> ss {debug=b, verbose=b})
              , ("excludearchives", \ss b -> ss {includeArchives=not b})
              , ("excludehidden", \ss b -> ss {includeHidden=not b})
              , ("followsymlinks", \ss b -> ss {followSymlinks=b})
              , ("help", \ss b -> ss {printUsage=b})
              , ("includearchives", \ss b -> ss {includeArchives=b})
              , ("includehidden", \ss b -> ss {includeHidden=b})
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

shortToLong :: [FindOption] -> String -> Either String String
shortToLong _ "" = Left "Missing argument"
shortToLong opts s | length s == 2 && head s == '-' =
                      if any (\so -> short so == Just (tail s)) optsWithShort
                      then Right $ "--" ++ getLongForShort s
                      else Left $ "Invalid option: " ++ tail s ++ "\n"
                   | otherwise = Right s
  where optsWithShort = filter (isJust . short) opts
        getLongForShort x = (long . head . filter (\so -> short so == Just (tail x))) optsWithShort

updateSettingsFromJson :: FindSettings -> [FindOption] -> String -> IO (Either String FindSettings)
updateSettingsFromJson settings opts jsonStr = do
  case decode (BC.pack jsonStr) of
    Just json -> 
      case updateFindSettingsFromJsonValue settings json of
        Left e -> return $ Left e
        Right settings' -> return $ Right settings'
    Nothing -> return $ Left "Failed to parse JSON"

updateSettingsFromFile :: FindSettings -> [FindOption] -> FilePath -> IO (Either String FindSettings)
updateSettingsFromFile settings opts filePath = do
  isFile' <- isFile filePath
  if isFile'
  then if ".json" `isSuffixOf` filePath
       then do
          settingsJsonStringEither <- getFileString filePath
          case settingsJsonStringEither of
            Left e -> return $ Left e
            Right settingsJsonString -> updateSettingsFromJson settings opts settingsJsonString
       else return $ Left "Settings file must be a json file\n"
  else return $ Left $ "Settings file not found: " ++ filePath ++ "\n"

updateSettingsFromArgs :: FindSettings -> [FindOption] -> [String] -> Either String FindSettings
updateSettingsFromArgs settings opts arguments =
  if any isLeft longArgs
  then (Left . head . lefts) longArgs
  else
    recSettingsFromArgs settings $ rights longArgs
  where recSettingsFromArgs :: FindSettings -> [String] -> Either String FindSettings
        recSettingsFromArgs ss args =
          case args of
          [] -> Right ss
          [a] | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              BoolActionType -> recSettingsFromArgs (getBoolAction (argName a) ss True) []
              StringActionType -> Left $ "Missing value for option: " ++ a ++ "\n"
              IntegerActionType -> Left $ "Missing value for option: " ++ a ++ "\n"
              UnknownActionType -> Left $ "Invalid option: " ++ a ++ "\n"
          a:as | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              BoolActionType -> recSettingsFromArgs (getBoolAction (argName a) ss True) as
              StringActionType -> recSettingsFromArgs (getStringAction (argName a) ss (head as)) (tail as)
              IntegerActionType -> recSettingsFromArgs (getIntegerAction (argName a) ss (read (head as))) (tail as)
              UnknownActionType -> Left $ "Invalid option: " ++ argName a ++ "\n"
          a:as -> recSettingsFromArgs (ss {paths = paths ss ++ [a]}) as
        longArgs :: [Either String String]
        longArgs = map (shortToLong opts) arguments
        getActionType :: String -> ActionType
        getActionType a
          | isBoolAction a = BoolActionType
          | isStringAction a = StringActionType
          | isIntegerAction a = IntegerActionType
          | otherwise = UnknownActionType
        argName :: String -> String
        argName = dropWhile (=='-')
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

settingsFromArgs :: [FindOption] -> [String] -> Either String FindSettings
settingsFromArgs = updateSettingsFromArgs defaultFindSettings{printFiles=True}

ioUpdateSettingsFromArgs :: FindSettings -> [FindOption] -> [String] -> IO (Either String FindSettings)
ioUpdateSettingsFromArgs settings opts arguments = do
  if "--settings-file" `elem` arguments
    then do
      let beforeSettingsFileArgs = takeWhile (/="--settings-file") arguments
      let settingsFileArgs = take 1 $ tail $ dropWhile (/="--settings-file") arguments
      let afterSettingsFileArgs = drop 2 $ dropWhile (/="--settings-file") arguments
      let settingsEither1 = updateSettingsFromArgs settings opts beforeSettingsFileArgs
      case settingsEither1 of
        Left e -> return $ Left e
        Right settings1 -> do
          case settingsFileArgs of
            [] -> do
              return $ updateSettingsFromArgs settings1 opts afterSettingsFileArgs
            [f] -> do
              settingsEither2 <- updateSettingsFromFile settings1 opts f
              case settingsEither2 of
                Left e -> return $ Left e
                Right settings2 -> do
                  return $ updateSettingsFromArgs settings2 opts afterSettingsFileArgs
    else return $ updateSettingsFromArgs settings opts arguments

ioSettingsFromArgs :: [FindOption] -> [String] -> IO (Either String FindSettings)
ioSettingsFromArgs = ioUpdateSettingsFromArgs defaultFindSettings{printFiles=True}
