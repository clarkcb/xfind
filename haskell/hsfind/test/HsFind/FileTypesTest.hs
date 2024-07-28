module HsFind.FileTypesTest
  (
    getFileTypeTests
  , getFileTypeFromNameTests
  , getIsFileTypeTests
  ) where

import Database.SQLite.Simple (Connection, open)

import HsFind.Config (getXfindDbPath)
import HsFind.FileTypes

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

getFileTypeTests :: IO [Test]
getFileTypeTests = do
  xfindDbPath <- getXfindDbPath
  conn <- open xfindDbPath
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
  archiveFileType <- getDbFileType conn archiveFile
  audioFileType <- getDbFileType conn audioFile
  binaryFileType <- getDbFileType conn binaryFile
  codeFileType <- getDbFileType conn codeFile
  fontFileType <- getDbFileType conn fontFile
  imageFileType <- getDbFileType conn imageFile
  textFileType <- getDbFileType conn textFile
  videoFileType <- getDbFileType conn videoFile
  xmlFileType <- getDbFileType conn xmlFile
  unknownFileType <- getDbFileType conn unknownFile
  return [ testCase "getDbFileType conn archive.zip == Archive" (archiveFileType @?= Archive)
         , testCase "getDbFileType conn music.mp3 == Audio" (audioFileType @?= Audio)
         , testCase "getDbFileType conn binary.exe == Binary" (binaryFileType @?= Binary)
         , testCase "getDbFileType conn FileTypes.hs == Code" (codeFileType @?= Code)
         , testCase "getDbFileType conn font.ttf == Font" (fontFileType @?= Font)
         , testCase "getDbFileType conn image.png == Image" (imageFileType @?= Image)
         , testCase "getDbFileType conn text.txt == Text" (textFileType @?= Text)
         , testCase "getDbFileType conn movie.mp4 == Video" (videoFileType @?= Video)
         , testCase "getDbFileType conn markup.xml == Xml" (xmlFileType @?= Xml)
         , testCase "getDbFileType conn unknown.xyz == Unknown" (unknownFileType @?= Unknown)
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

getIsFileTypeTests :: IO [Test]
getIsFileTypeTests = do
  xfindDbPath <- getXfindDbPath
  conn <- open xfindDbPath
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
  isArchiveFileType <- isArchiveFilePath conn archiveFile
  isAudioFileType <- isAudioFilePath conn audioFile
  isBinaryFileType <- isBinaryFilePath conn binaryFile
  isCodeFileType <- isCodeFilePath conn codeFile
  isFontFileType <- isFontFilePath conn fontFile
  isImageFileType <- isImageFilePath conn imageFile
  isTextFileType <- isTextFilePath conn textFile
  isCodeFileTextFileType <- isTextFilePath conn codeFile
  isXmlFileTextFileType <- isTextFilePath conn xmlFile
  isVideoFileType <- isVideoFilePath conn videoFile
  isXmlFileType <- isXmlFilePath conn xmlFile
  isUnknownFileType <- isUnknownFilePath conn unknownFile
  return [ testCase "isArchiveFilePath conn archive.zip == True" (isArchiveFileType @?= True)
         , testCase "isAudioFilePath conn music.mp3 == True" (isAudioFileType @?= True)
         , testCase "isBinaryFilePath conn binary.exe == True" (isBinaryFileType @?= True)
         , testCase "isCodeFilePath conn FileTypes.hs == True" (isCodeFileType @?= True)
         , testCase "isFontFilePath conn font.ttf == True" (isFontFileType @?= True)
         , testCase "isImageFilePath conn image.png == True" (isImageFileType @?= True)
         , testCase "isTextFilePath conn text.txt == True" (isTextFileType @?= True)
         , testCase "isTextFilePath conn FileTypes.hs == True" (isCodeFileTextFileType @?= True)
         , testCase "isTextFilePath conn markup.xml == True" (isXmlFileTextFileType @?= True)
         , testCase "isVideoFilePath conn movie.mp4 == True" (isVideoFileType @?= True)
         , testCase "isXmlFilePath conn markup.xml == True" (isXmlFileType @?= True)
         , testCase "isUnknownFilePath conn unknown.xyz == True" (isUnknownFileType @?= True)
         ]
