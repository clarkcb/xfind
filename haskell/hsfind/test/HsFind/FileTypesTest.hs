module HsFind.FileTypesTest
  (
    getFileTypeTests
  , getFileTypeFromNameTests
  ) where

import HsFind.FileTypes

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

getFileTypeTests :: IO [Test]
getFileTypeTests = do
  let archiveFile = "archive.zip"
  let audioFile = "music.mp3"
  let binaryFile = "binary.exe"
  let codeFile = "FileTypes.hs"
  let fontFile = "font.ttf"
  let imageFile = "image.png"
  let textFile = "text.txt"
  let videoFile = "movie.mp4"
  let xmlFile = "markup.xml"
  let unknownFile = "unknown.xyz"
  jsonFileTypes <- getJsonFileTypes
  let archiveFileType = getFileTypeFromJsonFileTypes jsonFileTypes archiveFile
  let audioFileType = getFileTypeFromJsonFileTypes jsonFileTypes audioFile
  let binaryFileType = getFileTypeFromJsonFileTypes jsonFileTypes binaryFile
  let codeFileType = getFileTypeFromJsonFileTypes jsonFileTypes codeFile
  let fontFileType = getFileTypeFromJsonFileTypes jsonFileTypes fontFile
  let imageFileType = getFileTypeFromJsonFileTypes jsonFileTypes imageFile
  let textFileType = getFileTypeFromJsonFileTypes jsonFileTypes textFile
  let videoFileType = getFileTypeFromJsonFileTypes jsonFileTypes videoFile
  let xmlFileType = getFileTypeFromJsonFileTypes jsonFileTypes xmlFile
  let unknownFileType = getFileTypeFromJsonFileTypes jsonFileTypes unknownFile
  return [ testCase "getFileTypeFromJsonFileTypes jsonFileTypes archive.zip == Archive" (archiveFileType @?= Archive)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes music.mp3 == Audio" (audioFileType @?= Audio)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes binary.exe == Binary" (binaryFileType @?= Binary)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes FileTypes.hs == Code" (codeFileType @?= Code)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes font.ttf == Font" (fontFileType @?= Font)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes image.png == Image" (imageFileType @?= Image)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes text.txt == Text" (textFileType @?= Text)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes movie.mp4 == Video" (videoFileType @?= Video)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes markup.xml == Xml" (xmlFileType @?= Xml)
         , testCase "getFileTypeFromJsonFileTypes jsonFileTypes unknown.xyz == Unknown" (unknownFileType @?= Unknown)
         ]

getFileTypeFromNameTests :: IO [Test]
getFileTypeFromNameTests = do
  let archiveFileType = getFileTypeForName "archive"
  let audioFileType = getFileTypeForName "AUDIO"
  let binaryFileType = getFileTypeForName "binarY"
  let codeFileType = getFileTypeForName "cODe"
  let fontFileType = getFileTypeForName "Font"
  let imageFileType = getFileTypeForName "iMAGe"
  let textFileType = getFileTypeForName "Text"
  let videoFileType = getFileTypeForName "video"
  let xmlFileType = getFileTypeForName "XML"
  let unknownFileType = getFileTypeForName "whoknows"
  return [ testCase "getFileTypeForName archive == Archive" (archiveFileType @?= Archive)
         , testCase "getFileTypeForName AUDIO == Audio" (audioFileType @?= Audio)
         , testCase "getFileTypeForName binarY == Binary" (binaryFileType @?= Binary)
         , testCase "getFileTypeForName cODe == Code" (codeFileType @?= Code)
         , testCase "getFileTypeForName Font == Font" (fontFileType @?= Font)
         , testCase "getFileTypeForName iMAGe == Image" (imageFileType @?= Image)
         , testCase "getFileTypeForName Text == Text" (textFileType @?= Text)
         , testCase "getFileTypeForName video == Video" (videoFileType @?= Video)
         , testCase "getFileTypeForName XML == Xml" (xmlFileType @?= Xml)
         , testCase "getFileTypeForName whoknows == Unknown" (unknownFileType @?= Unknown)
         ]
