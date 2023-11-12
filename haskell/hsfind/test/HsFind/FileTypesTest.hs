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
  archiveFileType <- getFileType archiveFile
  audioFileType <- getFileType audioFile
  binaryFileType <- getFileType binaryFile
  codeFileType <- getFileType codeFile
  fontFileType <- getFileType fontFile
  imageFileType <- getFileType imageFile
  textFileType <- getFileType textFile
  videoFileType <- getFileType videoFile
  xmlFileType <- getFileType xmlFile
  unknownFileType <- getFileType unknownFile
  return [ testCase "getFileType archive.zip == Archive" (archiveFileType @?= Archive)
         , testCase "getFileType music.mp3 == Audio" (audioFileType @?= Audio)
         , testCase "getFileType binary.exe == Binary" (binaryFileType @?= Binary)
         , testCase "getFileType FileTypes.hs == Code" (codeFileType @?= Code)
         , testCase "getFileType font.ttf == Font" (fontFileType @?= Font)
         , testCase "getFileType image.png == Image" (imageFileType @?= Image)
         , testCase "getFileType text.txt == Text" (textFileType @?= Text)
         , testCase "getFileType movie.mp4 == Video" (videoFileType @?= Video)
         , testCase "getFileType markup.xml == Xml" (xmlFileType @?= Xml)
         , testCase "getFileType unknown.xyz == Unknown" (unknownFileType @?= Unknown)
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
