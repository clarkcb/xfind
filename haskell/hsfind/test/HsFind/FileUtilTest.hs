module HsFind.FileUtilTest (getFileUtilTests) where

import HsFind.Config (getHome)
import HsFind.FileUtil

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

getFileUtilTests :: IO [Test]
getFileUtilTests = do
  userPath <- getHome
  expandedTilde <- expandPath "~"
  expandedTildeSlash <- expandPath "~/"
  expandedTildePath <- expandPath "~/src/xfind"
  expandedTildeNamePath <- expandPath "~cary/src/xfind"
  return [ testCase "expandPath ~" (expandedTilde @?= userPath) 
         , testCase "expandPath ~/" (expandedTildeSlash @?= userPath)
         , testCase "expandPath ~/src/xfind" (expandedTildePath @?= userPath ++ "/src/xfind")
         , testCase "expandPath ~cary/src/xfind" (expandedTildeNamePath @?= userPath ++ "/src/xfind")

         , testCase "getExtension file.txt" (getExtension "file.txt" @?= Just "txt")
         , testCase "getExtension file." (getExtension "file." @?= Nothing)
         , testCase "getExtension file" (getExtension "file" @?= Nothing)
         , testCase "getExtension .file.txt" (getExtension ".file.txt" @?= Just "txt")
         , testCase "getExtension .file." (getExtension ".file." @?= Nothing)
         , testCase "getExtension .file" (getExtension ".file" @?= Nothing)

         , testCase "normalizeExtension .txt" (normalizeExtension ".txt" @?= "txt")
         , testCase "normalizeExtension txt" (normalizeExtension "txt" @?= "txt")
         , testCase "normalizeExtension .TXT" (normalizeExtension ".TXT" @?= "txt")
         , testCase "normalizeExtension TXT" (normalizeExtension "TXT" @?= "txt")

         , testCase "hasExtension file.txt .txt" (hasExtension "file.txt" "txt" @?= True)
         , testCase "hasExtension file.txt .zip" (hasExtension "file.txt" "zip" @?= False)

         , testCase "isDotDir ." (isDotDir "." @?= True)
         , testCase "isDotDir .." (isDotDir ".." @?= True)
         , testCase "isDotDir .git" (isDotDir ".git" @?= False)

         , testCase "isHiddenFilePath file.txt" (isHiddenFilePath "file.txt" @?= False)
         , testCase "isHiddenFilePath .file.txt" (isHiddenFilePath ".file.txt" @?= True)
         , testCase "isHiddenFilePath .git" (isHiddenFilePath ".git" @?= True)
         ]
