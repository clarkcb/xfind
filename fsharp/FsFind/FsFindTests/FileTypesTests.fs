namespace FsFindTests

open NUnit.Framework
open FsFindLib

[<TestFixture>]
type FileTypesTests () =

    member this.FileTypes = FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.GetFileType_ArchiveFile_FileTypeArchive () =
        let archiveFile = "archive.zip"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(archiveFile), Is.EqualTo(FileType.Archive))
        ()

    [<Test>]
    member this.GetFileType_AudioFile_FileTypeAudio () =
        let audioFile = "music.mp3"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(audioFile), Is.EqualTo(FileType.Audio))
        ()

    [<Test>]
    member this.GetFileType_BinaryFile_FileTypeBinary () =
        let binaryFile = "binary.exe"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(binaryFile), Is.EqualTo(FileType.Binary))
        ()

    [<Test>]
    member this.GetFileType_CodeFile_FileTypeCode () =
        let codeFile = "code.cs"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(codeFile), Is.EqualTo(FileType.Code))
        ()

    [<Test>]
    member this.GetFileType_FontFile_FileTypeFont () =
        let fontFile = "font.ttf"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(fontFile), Is.EqualTo(FileType.Font))
        ()

    [<Test>]
    member this.GetFileType_ImageFile_FileTypeImage () =
        let imageFile = "image.png"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(imageFile), Is.EqualTo(FileType.Image))
        ()

    [<Test>]
    member this.GetFileType_TextFile_FileTypeText () =
        let textFile = "text.txt"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(textFile), Is.EqualTo(FileType.Text))
        ()

    [<Test>]
    member this.GetFileType_VideoFile_FileTypeVideo () =
        let videoFile = "movie.mp4"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(videoFile), Is.EqualTo(FileType.Video))
        ()

    [<Test>]
    member this.GetFileType_XmlFile_FileTypeXml () =
        let xmlFile = "markup.xml"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(xmlFile), Is.EqualTo(FileType.Xml))
        ()

    [<Test>]
    member this.GetFileType_UnknownFile_FileTypeUnknown () =
        let unknownFile = "unknown.xyz"
        Assert.That(this.FileTypes.GetFileTypeForFilePath(unknownFile), Is.EqualTo(FileType.Unknown))
        ()

    [<Test>]
    member this.IsArchiveFile_ArchiveFile_True () =
        let archiveFile = "archive.zip"
        Assert.That(this.FileTypes.IsArchiveFilePath(archiveFile))
        ()

    [<Test>]
    member this.IsAudioFile_AudioFile_True () =
        let audioFile = "music.mp3"
        Assert.That(this.FileTypes.IsAudioFilePath(audioFile))
        ()

    [<Test>]
    member this.IsBinaryFile_BinaryFile_True () =
        let binaryFile = "binary.exe"
        Assert.That(this.FileTypes.IsBinaryFilePath(binaryFile))
        ()

    [<Test>]
    member this.IsCodeFile_CodeFile_True () =
        let codeFile = "code.cs"
        Assert.That(this.FileTypes.IsCodeFilePath(codeFile))
        ()

    [<Test>]
    member this.IsFontFile_FontFile_True () =
        let fontFile = "font.ttf"
        Assert.That(this.FileTypes.IsFontFilePath(fontFile))
        ()

    [<Test>]
    member this.IsImageFile_ImageFile_True () =
        let imageFile = "image.png"
        Assert.That(this.FileTypes.IsImageFilePath(imageFile))
        ()

    [<Test>]
    member this.IsTextFile_TextFile_True () =
        let textFile = "text.txt"
        Assert.That(this.FileTypes.IsTextFilePath(textFile))
        ()

    [<Test>]
    member this.IsVideoFile_VideoFile_True () =
        let videoFile = "movie.mp4"
        Assert.That(this.FileTypes.IsVideoFilePath(videoFile))
        ()

    [<Test>]
    member this.IsXmlFile_XmlFile_True () =
        let xmlFile = "markup.xml"
        Assert.That(this.FileTypes.IsXmlFilePath(xmlFile))
        ()

    // [<Test>]
    // member this.IsSearchableFile_XmlFile_True () =
    //     let xmlFile = "markup.xml"
    //     Assert.IsTrue(this.FileTypes.IsSearchableFile(xmlFile))
    //     ()
