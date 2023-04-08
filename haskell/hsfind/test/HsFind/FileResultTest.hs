module HsFind.FileResultTest
  ( getFileResultTests
  , getFileResultWithSizeTests
  ) where

import HsFind.FileResult
import HsFind.FileTypes (FileType(..))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getFileResultTests :: IO [Test]
getFileResultTests = do
  let fileResult = newFileResult "./FileResult.hs" Code
  return [ testCase "fileResultPath == ./FileResult.hs" (fileResultPath fileResult @?= "./FileResult.hs")
         , testCase "fileResultCode == Code" (fileResultType fileResult @?= Code)
         , testCase "fileResultSize == 0" (fileResultSize fileResult @?= 0)
         ]

getFileResultWithSizeTests :: IO [Test]
getFileResultWithSizeTests = do
  let fileResult = newFileResultWithSize "./FileResult.hs" Code 1000
  return [ testCase "fileResultPath == ./FileResult.hs" (fileResultPath fileResult @?= "./FileResult.hs")
         , testCase "fileResultCode == Code" (fileResultType fileResult @?= Code)
         , testCase "fileResultSize == 1000" (fileResultSize fileResult @?= 1000)
         ]
