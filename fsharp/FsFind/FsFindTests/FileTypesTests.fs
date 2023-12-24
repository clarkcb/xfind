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
        Assert.That(this.FileTypes.GetFileType(archiveFile), Is.EqualTo(FileType.Archive))
        ()

    [<Test>]
    member this.GetFileType_AudioFile_FileTypeAudio () =
        let audioFile = FileInfo("music.mp3")
        Assert.That(this.FileTypes.GetFileType(audioFile), Is.EqualTo(FileType.Audio))
        ()

    [<Test>]
    member this.GetFileType_BinaryFile_FileTypeBinary () =
        let binaryFile = FileInfo("binary.exe")
        Assert.That(this.FileTypes.GetFileType(binaryFile), Is.EqualTo(FileType.Binary))
        ()

    [<Test>]
    member this.GetFileType_CodeFile_FileTypeCode () =
        let codeFile = FileInfo("code.cs")
        Assert.That(this.FileTypes.GetFileType(codeFile), Is.EqualTo(FileType.Code))
        ()

    [<Test>]
    member this.GetFileType_FontFile_FileTypeFont () =
        let fontFile = FileInfo("font.ttf")
        Assert.That(this.FileTypes.GetFileType(fontFile), Is.EqualTo(FileType.Font))
        ()

    [<Test>]
    member this.GetFileType_ImageFile_FileTypeImage () =
        let imageFile = FileInfo("image.png")
        Assert.That(this.FileTypes.GetFileType(imageFile), Is.EqualTo(FileType.Image))
        ()

    [<Test>]
    member this.GetFileType_TextFile_FileTypeText () =
        let textFile = FileInfo("text.txt")
        Assert.That(this.FileTypes.GetFileType(textFile), Is.EqualTo(FileType.Text))
        ()

    [<Test>]
    member this.GetFileType_VideoFile_FileTypeVideo () =
        let videoFile = FileInfo("movie.mp4")
        Assert.That(this.FileTypes.GetFileType(videoFile), Is.EqualTo(FileType.Video))
        ()

    [<Test>]
    member this.GetFileType_XmlFile_FileTypeXml () =
        let xmlFile = FileInfo("markup.xml")
        Assert.That(this.FileTypes.GetFileType(xmlFile), Is.EqualTo(FileType.Xml))
        ()

    [<Test>]
    member this.GetFileType_UnknownFile_FileTypeUnknown () =
        let unknownFile = FileInfo("unknown.xyz")
        Assert.That(this.FileTypes.GetFileType(unknownFile), Is.EqualTo(FileType.Unknown))
        ()

    [<Test>]
    member this.IsArchiveFile_ArchiveFile_True () =
        let archiveFile = FileInfo("archive.zip")
        Assert.That(this.FileTypes.IsArchiveFile(archiveFile))
        ()

    [<Test>]
    member this.IsAudioFile_AudioFile_True () =
        let audioFile = FileInfo("music.mp3")
        Assert.That(this.FileTypes.IsAudioFile(audioFile))
        ()

    [<Test>]
    member this.IsBinaryFile_BinaryFile_True () =
        let binaryFile = FileInfo("binary.exe")
        Assert.That(this.FileTypes.IsBinaryFile(binaryFile))
        ()

    [<Test>]
    member this.IsCodeFile_CodeFile_True () =
        let codeFile = FileInfo("code.cs")
        Assert.That(this.FileTypes.IsCodeFile(codeFile))
        ()

    [<Test>]
    member this.IsFontFile_FontFile_True () =
        let fontFile = FileInfo("font.ttf")
        Assert.That(this.FileTypes.IsFontFile(fontFile))
        ()

    [<Test>]
    member this.IsImageFile_ImageFile_True () =
        let imageFile = FileInfo("image.png")
        Assert.That(this.FileTypes.IsImageFile(imageFile))
        ()

    [<Test>]
    member this.IsTextFile_TextFile_True () =
        let textFile = FileInfo("text.txt")
        Assert.That(this.FileTypes.IsTextFile(textFile))
        ()

    [<Test>]
    member this.IsVideoFile_VideoFile_True () =
        let videoFile = FileInfo("movie.mp4")
        Assert.That(this.FileTypes.IsVideoFile(videoFile))
        ()

    [<Test>]
    member this.IsXmlFile_XmlFile_True () =
        let xmlFile = FileInfo("markup.xml")
        Assert.That(this.FileTypes.IsXmlFile(xmlFile))
        ()

    // [<Test>]
    // member this.IsSearchableFile_XmlFile_True () =
    //     let xmlFile = FileInfo("markup.xml")
    //     Assert.IsTrue(this.FileTypes.IsSearchableFile(xmlFile))
    //     ()
