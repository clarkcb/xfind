{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction #-}
module HsFind.FindOptions (
    FindOption(..)
  , getFindOptions
  , getUsage
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
  "Usage:\n hsfind [options] -s <findpattern> <startpath>\n\nOptions:\n" ++
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

data ActionType = ArgActionType
                | BoolFlagActionType
                | FlagActionType
                | UnknownActionType
  deriving (Show, Eq)

type ArgAction = FindSettings -> String -> FindSettings
type BoolFlagAction = FindSettings -> Bool -> FindSettings
type FlagAction = FindSettings -> FindSettings

argActions :: [(String, ArgAction)]
argActions = [ ("encoding", \ss s -> ss {textFileEncoding=s})
             , ("in-archiveext", \ss s -> ss {inArchiveExtensions = inArchiveExtensions ss ++ newExtensions s})
             , ("in-archivefilepattern", \ss s -> ss {inArchiveFilePatterns = inArchiveFilePatterns ss ++ [s]})
             , ("in-dirpattern", \ss s -> ss {inDirPatterns = inDirPatterns ss ++ [s]})
             , ("in-ext", \ss s -> ss {inExtensions = inExtensions ss ++ newExtensions s})
             , ("in-filepattern", \ss s -> ss {inFilePatterns = inFilePatterns ss ++ [s]})
             , ("in-filetype", \ss s -> ss {inFileTypes = inFileTypes ss ++ [getFileTypeForName s]})
             , ("in-linesafterpattern", \ss s -> ss {inLinesAfterPatterns = inLinesAfterPatterns ss ++ [s]})
             , ("in-linesbeforepattern", \ss s -> ss {inLinesBeforePatterns = inLinesBeforePatterns ss ++ [s]})
             , ("linesafter", \ss s -> ss {linesAfter = read s})
             , ("linesaftertopattern", \ss s -> ss {linesAfterToPatterns = linesAfterToPatterns ss ++ [s]})
             , ("linesafteruntilpattern", \ss s -> ss {linesAfterUntilPatterns = linesAfterUntilPatterns ss ++ [s]})
             , ("linesbefore", \ss s -> ss {linesBefore = read s})
             , ("maxlinelength", \ss s -> ss {maxLineLength = read s})
             , ("out-archiveext", \ss s -> ss {outArchiveExtensions = outArchiveExtensions ss ++ newExtensions s})
             , ("out-archivefilepattern", \ss s -> ss {outArchiveFilePatterns = outArchiveFilePatterns ss ++ [s]})
             , ("out-dirpattern", \ss s -> ss {outDirPatterns = outDirPatterns ss ++ [s]})
             , ("out-ext", \ss s -> ss {outExtensions = outExtensions ss ++ newExtensions s})
             , ("out-filepattern", \ss s -> ss {outFilePatterns = outFilePatterns ss ++ [s]})
             , ("out-filetype", \ss s -> ss {outFileTypes = outFileTypes ss ++ [getFileTypeForName s]})
             , ("out-linesafterpattern", \ss s -> ss {outLinesAfterPatterns = outLinesAfterPatterns ss ++ [s]})
             , ("out-linesbeforepattern", \ss s -> ss {outLinesBeforePatterns = outLinesBeforePatterns ss ++ [s]})
             , ("findpattern", \ss s -> ss {findPatterns = findPatterns ss ++ [s]})
             ]

flagActions :: [(String, FlagAction)]
flagActions = [ ("allmatches", \ss -> ss {firstMatch=False})
              , ("archivesonly", \ss -> ss {archivesOnly=True,
                                            findArchives=True})
              , ("colorize", \ss -> ss {colorize=True})
              , ("debug", \ss -> ss {debug=True, verbose=True})
              , ("excludehidden", \ss -> ss {excludeHidden=True})
              , ("firstmatch", \ss -> ss {firstMatch=True})
              , ("help", \ss -> ss {printUsage=True})
              , ("includehidden", \ss -> ss {excludeHidden=False})
              , ("listdirs", \ss -> ss {listDirs=True})
              , ("listfiles", \ss -> ss {listFiles=True})
              , ("listlines", \ss -> ss {listLines=True})
              , ("multilineoption-REMOVE", \ss -> ss {multiLineFind=True})
              , ("nocolorize", \ss -> ss {colorize=False})
              , ("noprintmatches", \ss -> ss {printResults=False})
              , ("norecursive", \ss -> ss {recursive=False})
              , ("nofindarchives", \ss -> ss {findArchives=False})
              , ("printmatches", \ss -> ss {printResults=True})
              , ("recursive", \ss -> ss {recursive=True})
              , ("findarchives", \ss -> ss {findArchives=True})
              , ("uniquelines", \ss -> ss {uniqueLines=True})
              , ("verbose", \ss -> ss {verbose=True})
              , ("version", \ss -> ss {printVersion=True})
              ]

boolFlagActions :: [(String, BoolFlagAction)]
boolFlagActions = [ ("allmatches", \ss b -> ss {firstMatch=not b})
                  , ("archivesonly", \ss b -> ss {archivesOnly=b,
                                                  findArchives=b})
                  , ("colorize", \ss b -> ss {colorize=b})
                  , ("debug", \ss b -> ss {debug=b, verbose=b})
                  , ("excludehidden", \ss b -> ss {excludeHidden=b})
                  , ("firstmatch", \ss b -> ss {firstMatch=b})
                  , ("help", \ss b -> ss {printUsage=b})
                  , ("includehidden", \ss b -> ss {excludeHidden=not b})
                  , ("listdirs", \ss b -> ss {listDirs=b})
                  , ("listfiles", \ss b -> ss {listFiles=b})
                  , ("listlines", \ss b -> ss {listLines=b})
                  , ("multilineoption-REMOVE", \ss b -> ss {multiLineFind=b})
                  , ("nocolorize", \ss b -> ss {colorize=not b})
                  , ("noprintmatches", \ss b -> ss {printResults=not b})
                  , ("norecursive", \ss b -> ss {recursive=not b})
                  , ("nofindarchives", \ss b -> ss {findArchives=not b})
                  , ("printmatches", \ss b -> ss {printResults=b})
                  , ("recursive", \ss b -> ss {recursive=b})
                  , ("findarchives", \ss b -> ss {findArchives=b})
                  , ("uniquelines", \ss b -> ss {uniqueLines=b})
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
    recSettingsFromArgs defaultFindSettings $ rights longArgs
  where recSettingsFromArgs :: FindSettings -> [String] -> Either String FindSettings
        recSettingsFromArgs settings args =
          case args of
          [] -> Right settings
          [a] | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              ArgActionType -> Left $ "Missing value for option: " ++ a ++ "\n"
              BoolFlagActionType -> recSettingsFromArgs (getBoolFlagAction (argName a) settings True) []
              FlagActionType -> recSettingsFromArgs (getFlagAction (argName a) settings) []
              UnknownActionType -> Left $ "Invalid option: " ++ a ++ "\n"
          a:as | "-" `isPrefixOf` a ->
            case getActionType (argName a) of
              ArgActionType -> recSettingsFromArgs (getArgAction (argName a) settings (head as)) (tail as)
              BoolFlagActionType -> recSettingsFromArgs (getBoolFlagAction (argName a) settings True) as
              FlagActionType -> recSettingsFromArgs (getFlagAction (argName a) settings) as
              UnknownActionType -> Left $ "Invalid option: " ++ argName a ++ "\n"
          a:as -> recSettingsFromArgs (settings {startPath=a}) as
        longArgs :: [Either String String]
        longArgs = map (shortToLong opts) arguments
        getActionType :: String -> ActionType
        getActionType a
          | isArgAction a = ArgActionType
          | isBoolFlagAction a = BoolFlagActionType
          | isFlagAction a = FlagActionType
          | otherwise = UnknownActionType
        argName :: String -> String
        argName = dropWhile (=='-')
        getArgAction :: String -> ArgAction
        getArgAction a = snd $ head $ filter (\(x,_) -> a==x) argActions
        getBoolFlagAction :: String -> BoolFlagAction
        getBoolFlagAction a = snd $ head $ filter (\(x,_) -> a==x) boolFlagActions
        getFlagAction :: String -> FlagAction
        getFlagAction a = snd $ head $ filter (\(x,_) -> a==x) flagActions
        isArgAction :: String -> Bool
        isArgAction a = isJust $ lookup a argActions
        isBoolFlagAction :: String -> Bool
        isBoolFlagAction a = isJust $ lookup a boolFlagActions
        isFlagAction :: String -> Bool
        isFlagAction a = isJust $ lookup a flagActions
