namespace FsFindTests

open System.IO
open NUnit.Framework
open FsFind

[<TestFixture>]
type FileTypesTests () =

    member this.FileTypes = FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.GetFileType_ArchiveFile_FileTypeArchive () =
        let archiveFile = FileInfo("archive.zip")
        Assert.AreEqual(FileType.Archive, this.FileTypes.GetFileType(archiveFile))
        ()

    [<Test>]
    member this.GetFileType_AudioFile_FileTypeAudio () =
        let audioFile = FileInfo("music.mp3")
        Assert.AreEqual(FileType.Audio, this.FileTypes.GetFileType(audioFile))
        ()

    [<Test>]
    member this.GetFileType_BinaryFile_FileTypeBinary () =
        let binaryFile = FileInfo("binary.exe")
        Assert.AreEqual(FileType.Binary, this.FileTypes.GetFileType(binaryFile))
        ()

    [<Test>]
    member this.GetFileType_CodeFile_FileTypeCode () =
        let codeFile = FileInfo("code.cs")
        Assert.AreEqual(FileType.Code, this.FileTypes.GetFileType(codeFile))
        ()

    [<Test>]
    member this.GetFileType_FontFile_FileTypeFont () =
        let fontFile = FileInfo("font.ttf")
        Assert.AreEqual(FileType.Font, this.FileTypes.GetFileType(fontFile))
        ()

    [<Test>]
    member this.GetFileType_ImageFile_FileTypeImage () =
        let imageFile = FileInfo("image.png")
        Assert.AreEqual(FileType.Image, this.FileTypes.GetFileType(imageFile))
        ()

    [<Test>]
    member this.GetFileType_TextFile_FileTypeText () =
        let textFile = FileInfo("text.txt")
        Assert.AreEqual(FileType.Text, this.FileTypes.GetFileType(textFile))
        ()

    [<Test>]
    member this.GetFileType_VideoFile_FileTypeVideo () =
        let videoFile = FileInfo("movie.mp4")
        Assert.AreEqual(FileType.Video, this.FileTypes.GetFileType(videoFile))
        ()

    [<Test>]
    member this.GetFileType_XmlFile_FileTypeXml () =
        let xmlFile = FileInfo("markup.xml")
        Assert.AreEqual(FileType.Xml, this.FileTypes.GetFileType(xmlFile))
        ()

    [<Test>]
    member this.GetFileType_UnknownFile_FileTypeUnknown () =
        let unknownFile = FileInfo("unknown.xyz")
        Assert.AreEqual(FileType.Unknown, this.FileTypes.GetFileType(unknownFile))
        ()

    [<Test>]
    member this.IsArchiveFile_ArchiveFile_True () =
        let archiveFile = FileInfo("archive.zip")
        Assert.IsTrue(this.FileTypes.IsArchiveFile(archiveFile))
        ()

    [<Test>]
    member this.IsAudioFile_AudioFile_True () =
        let audioFile = FileInfo("music.mp3")
        Assert.IsTrue(this.FileTypes.IsAudioFile(audioFile))
        ()

    [<Test>]
    member this.IsBinaryFile_BinaryFile_True () =
        let binaryFile = FileInfo("binary.exe")
        Assert.IsTrue(this.FileTypes.IsBinaryFile(binaryFile))
        ()

    [<Test>]
    member this.IsCodeFile_CodeFile_True () =
        let codeFile = FileInfo("code.cs")
        Assert.IsTrue(this.FileTypes.IsCodeFile(codeFile))
        ()

    [<Test>]
    member this.IsFontFile_FontFile_True () =
        let fontFile = FileInfo("font.ttf")
        Assert.IsTrue(this.FileTypes.IsFontFile(fontFile))
        ()

    [<Test>]
    member this.IsImageFile_ImageFile_True () =
        let imageFile = FileInfo("image.png")
        Assert.IsTrue(this.FileTypes.IsImageFile(imageFile))
        ()

    [<Test>]
    member this.IsTextFile_TextFile_True () =
        let textFile = FileInfo("text.txt")
        Assert.IsTrue(this.FileTypes.IsTextFile(textFile))
        ()

    [<Test>]
    member this.IsVideoFile_VideoFile_True () =
        let videoFile = FileInfo("movie.mp4")
        Assert.IsTrue(this.FileTypes.IsVideoFile(videoFile))
        ()

    [<Test>]
    member this.IsXmlFile_XmlFile_True () =
        let xmlFile = FileInfo("markup.xml")
        Assert.IsTrue(this.FileTypes.IsXmlFile(xmlFile))
        ()

    // [<Test>]
    // member this.IsSearchableFile_XmlFile_True () =
    //     let xmlFile = FileInfo("markup.xml")
    //     Assert.IsTrue(this.FileTypes.IsSearchableFile(xmlFile))
    //     ()
