{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction #-}
module HsFind.ArgTokenizer
  ( ArgToken(..)
  , ArgTokenizer(..)
  , ArgTokenType(..)
  , ArgValueType(..)
  , argTokenToString
  , tokenizeArgs
  , tokenizeFile
  , tokenizeHashMap
  , tokenizeJson) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.HashMap.Strict as HashMap (HashMap, keys, lookup)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Text (Text, unpack)
import Data.Vector as Vector (map, toList)
import GHC.Generics ( Generic )

import HsFind.FileUtil (expandPath, getFileString, isFile)


data ArgTokenType = ArgTokenTypeUnknown
            | ArgTokenTypeBool
            | ArgTokenTypeString
            | ArgTokenTypeInt
  deriving (Show, Eq)

data ArgValueType a b c = TypeA a | TypeB b | TypeC c

data ArgToken = ArgToken
  { name :: String
  , argType :: ArgTokenType
  , value :: ArgValueType Bool String Int
  } deriving (Generic)

argTokenToString :: ArgToken -> String
argTokenToString token = "{name=" ++ name token ++ ", value=" ++ valStr ++ "}"
  where
    valStr = case value token of
               TypeA b -> show b
               TypeB s -> show s
               TypeC i -> show i

data ArgTokenizer = ArgTokenizer
  { boolMap :: [(String, String)]
  , stringMap :: [(String, String)]
  , intMap :: [(String, String)]
  } deriving (Show, Eq, Generic)

longNameAndType :: ArgTokenizer -> String -> (String, ArgTokenType)
longNameAndType argTokenizer argName
  | isJust boolLongName = (fromJust boolLongName, ArgTokenTypeBool)
  | isJust stringLongName = (fromJust stringLongName, ArgTokenTypeString)
  | isJust intLongName = (fromJust intLongName, ArgTokenTypeInt)
  | otherwise = (argName, ArgTokenTypeUnknown)
  where
    boolLongName = Prelude.lookup argName $ boolMap argTokenizer
    stringLongName = Prelude.lookup argName $ stringMap argTokenizer
    intLongName = Prelude.lookup argName $ intMap argTokenizer

tokenizeArgs :: ArgTokenizer -> [String] -> Either String [ArgToken]
tokenizeArgs argTokenizer arguments = recTokenizeArgs argTokenizer arguments []
  where
    recTokenizeArgs :: ArgTokenizer -> [String] -> [ArgToken] -> Either String [ArgToken]
    recTokenizeArgs argTokenizer arguments argTokens =
      case arguments of
        [] -> Right argTokens
        a:as | "--" `isPrefixOf` a && '=' `elem` a -> procArg (argName a) (Just (argVal a)) True as argTokens
        a:as | "--" `isPrefixOf` a -> procArg (argName a) (listToMaybe as) False as argTokens
        a:as | "-" `isPrefixOf` a && length a > 2 -> procArg (take 1 (argName a)) Nothing False (("-" ++ drop 1 (argName a)) : as) argTokens
        a:as | "-" `isPrefixOf` a -> procArg (argName a) (listToMaybe as) False as argTokens
        a:as -> recTokenizeArgs argTokenizer as (argTokens ++ [ArgToken {name="path", argType=ArgTokenTypeString, value=TypeB a}])
    procArg :: String -> Maybe String -> Bool -> [String] -> [ArgToken] -> Either String [ArgToken]
    procArg arg val argEqualsVal args tokens =
      case longNameAndType argTokenizer arg of
        (lngName, ArgTokenTypeBool) ->
            recTokenizeArgs argTokenizer args (tokens ++ [ArgToken {name=lngName, argType=ArgTokenTypeBool, value=TypeA True}])
        (lngName, ArgTokenTypeString) ->
          if isJust val
            then if argEqualsVal
              then recTokenizeArgs argTokenizer args (tokens ++ [ArgToken {name=lngName, argType=ArgTokenTypeString, value=TypeB (fromJust val)}])
              else recTokenizeArgs argTokenizer (tail args) (tokens ++ [ArgToken {name=lngName, argType=ArgTokenTypeString, value=TypeB (fromJust val)}])
            else Left $ "Missing value for option: " ++ arg
        (lngName, ArgTokenTypeInt) ->
          if isJust val
            then recTokenizeArgs argTokenizer (tail args) (tokens ++ [ArgToken {name=lngName, argType=ArgTokenTypeInt, value=TypeC (read (fromJust val))}])
            else Left $ "Missing value for option: " ++ arg
        (_, _) -> Left $ "Invalid option from tokenizeArgs: " ++ arg
    argName :: String -> String
    argName a = takeWhile (/='=') (dropWhile (=='-') a)
    argVal :: String -> String
    argVal a = drop 1 (dropWhile (/='=') a)

tokenizeHashMap :: ArgTokenizer -> HashMap Text Value -> Either String [ArgToken]
tokenizeHashMap argTokenizer hashMap = recTokenizeHashMap (sort (keys hashMap)) []
  where
    recTokenizeHashMap :: [Text] -> [ArgToken] -> Either String [ArgToken]
    recTokenizeHashMap keysList tokens =
      case keysList of
        [] -> Right tokens
        k:ks ->
          case HashMap.lookup k hashMap of
            Just v -> 
              case getArgTokensForValue (unpack k) v of
                Left e -> Left e
                Right valTokens -> recTokenizeHashMap ks (tokens ++ valTokens)
            Nothing -> Left $ "Key not found in JSON: " ++ unpack k
    getArgTokensForValue :: String -> Value -> Either String [ArgToken]
    getArgTokensForValue key jsonVal =
      case jsonVal of
        Bool b ->
          if isBoolKey key
          then Right [ArgToken {name=key, argType=ArgTokenTypeBool, value=TypeA b}]
          else Left $ "Invalid boolean key in JSON: " ++ key
        String s ->
          if isStringKey key
          then Right [ArgToken {name=key, argType=ArgTokenTypeString, value=TypeB (unpack s)}]
          else Left $ "Invalid string key in JSON: " ++ key
        Array arr ->
          if isStringKey key
          then
            case getArgTokensForStringValues key (Vector.toList arr) [] of
              Left e -> Left e
              Right valTokens -> Right valTokens
          else Left $ "Invalid string key for array in JSON: " ++ key
        Number n ->
          if isIntKey key
          then Right [ArgToken {name=key, argType=ArgTokenTypeInt, value=TypeC (round n)}]
          else Left $ "Invalid integer key in JSON: " ++ key
        _ -> Left $ "Unsupported JSON value type for key: " ++ key
    isBoolKey :: String -> Bool
    isBoolKey k = isJust $ Prelude.lookup k $ boolMap argTokenizer
    isStringKey :: String -> Bool
    isStringKey k = isJust $ Prelude.lookup k $ stringMap argTokenizer
    isIntKey :: String -> Bool
    isIntKey k = isJust $ Prelude.lookup k $ intMap argTokenizer
    getArgTokensForStringValues :: String -> [Value] -> [ArgToken] -> Either String [ArgToken]
    getArgTokensForStringValues key strVals tokens =
      case strVals of
        [] -> Right tokens
        v:vs ->
          case getArgTokensForValue key v of
            Left e -> Left e
            Right valTokens -> getArgTokensForStringValues key vs (tokens ++ valTokens)

tokenizeJson :: ArgTokenizer -> String -> Either String [ArgToken]
tokenizeJson argTokenizer jsonStr =
  case maybeJsonVal of
    Just jsonVal -> 
      case jsonVal of
        Object obj -> tokenizeHashMap argTokenizer obj
        _ -> Left "Unable to parse JSON"
    Nothing -> Left "Unable to parse JSON"
  where maybeJsonVal = decode (BC.pack jsonStr) :: Maybe Value

tokenizeFile :: ArgTokenizer -> FilePath -> IO (Either String [ArgToken])
tokenizeFile argTokenizer filePath = do
  expandedPath <- expandPath filePath
  isFile' <- isFile expandedPath
  if isFile'
  then if ".json" `isSuffixOf` expandedPath
       then do
          settingsJsonStringEither <- getFileString expandedPath
          case settingsJsonStringEither of
            Left e -> return $ Left e
            Right settingsJsonString -> do
              return $ tokenizeJson argTokenizer settingsJsonString
       else return $ Left $ "Invalid settings file (must be JSON): " ++ filePath
  else return $ Left $ "Settings file not found: " ++ filePath
