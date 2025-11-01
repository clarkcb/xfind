module HsFind.ArgTokenizerTest
  ( getTokenizeArgsNoArgsTests
  , getTokenizeArgsWithArgsTests
  ) where

import HsFind.ArgTokenizer
import HsFind.FindOptions

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

getTokenizeArgsNoArgsTests :: IO [Test]
getTokenizeArgsNoArgsTests = do
  findOptionsEither <- getFindOptions
  case findOptionsEither of
    Left _ -> return [testCase "getTokenizeArgsNoArgsTests" (True @?= False)]
    Right findOptions -> do
      case tokenizeArgs (argTokenizer findOptions) [] of
        Left _ -> return [testCase "getTokenizeArgsNoArgsTests" (True @?= False)]
        Right tokens ->
          return [testCase "tokenizeArgs with no args results in zero tokens" (length tokens @?= 0)]

getTokenizeArgsWithArgsTests :: IO [Test]
getTokenizeArgsWithArgsTests = do
  let args = ["--debug","-x","hs","-X","hi,o","."]
  findOptionsEither <- getFindOptions
  case findOptionsEither of
    Left _ -> return [testCase "getTokenizeArgsWithArgsTests" (True @?= False)]
    Right findOptions -> do
      case tokenizeArgs (argTokenizer findOptions) args of
        Left _ -> return [testCase "getTokenizeArgsWithArgsTests" (True @?= False)]
        Right tokens ->
          return [ testCase "tokenizeArgs with multiple args results in multiple tokens" (length tokens @?= 4)
                 , testCase "first token name is debug" (name (head tokens) @?= "debug")
                 , testCase "first token argType is ArgTokenTypeBool" (argType (head tokens) @?= ArgTokenTypeBool)
                --  , testCase "first token value is TypeA True" (value (head tokens) @?= TypeA True)
                --  , testCase "first token is --debug" (name (head tokens) @?= ArgToken {name="debug", argType=ArgTokenTypeBool, value=TypeA True})
                 , testCase "second token name is in-ext" (name (tokens !! 1) @?= "in-ext")
                 , testCase "second token argType is ArgTokenTypeString" (argType (tokens !! 1) @?= ArgTokenTypeString)
                --  , testCase "second token is -x hs" (tokens !! 1 @?= ArgToken {name="inExtensions", argType=ArgTokenTypeString, value=TypeB "hs"})
                 , testCase "third token name is out-ext" (name (tokens !! 2) @?= "out-ext")
                 , testCase "third token argType is ArgTokenTypeString" (argType (tokens !! 2) @?= ArgTokenTypeString)
                --  , testCase "third token is -X hi,o" (tokens !! 2 @?= ArgToken {name="outExtensions", argType=ArgTokenTypeString, value=TypeB "hi,o"})
                 , testCase "fourth token name is path" (name (tokens !! 3) @?= "path")
                 , testCase "fourth token argType is ArgTokenTypeString" (argType (tokens !! 3) @?= ArgTokenTypeString)
                --  , testCase "fourth token is path ." (tokens !! 3 @?= ArgToken {name="path", argType=ArgTokenTypeString, value=TypeB "."})
                 ]
