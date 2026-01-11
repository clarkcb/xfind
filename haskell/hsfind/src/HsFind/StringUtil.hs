module HsFind.StringUtil
  ( compareStrings
  , padString
  , sliceString
  , trimLeadingWhitespace
  , trimTrailingWhitespace
  ) where

import Data.Char (isSpace, toLower)

compareStrings :: Bool -> String -> String -> Ordering
compareStrings caseInsensitive s1 s2 =
  if caseInsensitive
  then compare (lower s1) (lower s2)
  else compare s1 s2
  where lower = map toLower

padString :: String -> Int -> String
padString s len | length s < len = s ++ replicate (len - length s) ' '
                | otherwise      = s

-- sliceString startIdx endIdx s
-- Extracts a substring from 'startIdx' (inclusive) to 'endIdx' (exclusive) indices.
sliceString :: Int -> Int -> String -> String
sliceString startIdx endIdx s = take (endIdx - startIdx) (drop startIdx s)

trimLeadingWhitespace :: String -> String
trimLeadingWhitespace = dropWhile isSpace

trimTrailingWhitespace :: String -> String
trimTrailingWhitespace = reverse . trimLeadingWhitespace . reverse
