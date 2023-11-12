using System.IO;
using NUnit.Framework;
using CsFindLib;

namespace CsFindTests;

[TestFixture]
public class FileTypesTests
{
	private readonly FileTypes _fileTypes = new FileTypes();

	[Test]
	public void GetFileType_ArchiveFile_FileTypeArchive()
	{
		var archiveFile = new FileInfo("archive.zip");
		Assert.AreEqual(FileType.Archive, _fileTypes.GetFileType(archiveFile));
	}

	[Test]
	public void GetFileType_AudioFile_FileTypeAudio()
	{
		var audioFile = new FileInfo("music.mp3");
		Assert.AreEqual(FileType.Audio, _fileTypes.GetFileType(audioFile));
	}

	[Test]
	public void GetFileType_BinaryFile_FileTypeBinary()
	{
		var binaryFile = new FileInfo("binary.exe");
		Assert.AreEqual(FileType.Binary, _fileTypes.GetFileType(binaryFile));
	}

	[Test]
	public void GetFileType_CodeFile_FileTypeCode()
	{
		var codeFile = new FileInfo("code.cs");
		Assert.AreEqual(FileType.Code, _fileTypes.GetFileType(codeFile));
	}

	[Test]
	public void GetFileType_FontFile_FileTypeCode()
	{
		var fontFile = new FileInfo("font.ttf");
		Assert.AreEqual(FileType.Font, _fileTypes.GetFileType(fontFile));
	}

	[Test]
	public void GetFileType_ImageFile_FileTypeImage()
	{
		var imageFile = new FileInfo("image.png");
		Assert.AreEqual(FileType.Image, _fileTypes.GetFileType(imageFile));
	}

	[Test]
	public void GetFileType_TextFile_FileTypeText()
	{
		var textFile = new FileInfo("text.txt");
		Assert.AreEqual(FileType.Text, _fileTypes.GetFileType(textFile));
	}

	[Test]
	public void GetFileType_VideoFile_FileTypeVideo()
	{
		var videoFile = new FileInfo("movie.mp4");
		Assert.AreEqual(FileType.Video, _fileTypes.GetFileType(videoFile));
	}

	[Test]
	public void GetFileType_XmlFile_FileTypeXml()
	{
		var xmlFile = new FileInfo("markup.xml");
		Assert.AreEqual(FileType.Xml, _fileTypes.GetFileType(xmlFile));
	}

	[Test]
	public void GetFileType_UnknownFile_FileTypeUnknown()
	{
		var unknownFile = new FileInfo("unknown.xyz");
		Assert.AreEqual(FileType.Unknown, _fileTypes.GetFileType(unknownFile));
	}

	[Test]
	public void IsArchiveFile_ArchiveFile_True()
	{
		var archiveFile = new FileInfo("archive.zip");
		Assert.IsTrue(_fileTypes.IsArchiveFile(archiveFile));
	}

	[Test]
	public void IsAudioFile_AudioFile_True()
	{
		var audioFile = new FileInfo("music.mp3");
		Assert.IsTrue(_fileTypes.IsAudioFile(audioFile));
	}

	[Test]
	public void IsBinaryFile_BinaryFile_True()
	{
		var binaryFile = new FileInfo("binary.exe");
		Assert.IsTrue(_fileTypes.IsBinaryFile(binaryFile));
	}

	[Test]
	public void IsCodeFile_CodeFile_True()
	{
		var codeFile = new FileInfo("code.cs");
		Assert.IsTrue(_fileTypes.IsCodeFile(codeFile));
	}

	[Test]
	public void IsFontFile_FontFile_True()
	{
		var fontFile = new FileInfo("font.ttf");
		Assert.IsTrue(_fileTypes.IsFontFile(fontFile));
	}

	[Test]
	public void IsTextFile_ImageFile_True()
	{
		var imageFile = new FileInfo("image.png");
		Assert.IsTrue(_fileTypes.IsImageFile(imageFile));
	}

	[Test]
	public void IsTextFile_TextFile_True()
	{
		var textFile = new FileInfo("text.txt");
		Assert.IsTrue(_fileTypes.IsTextFile(textFile));
	}

	[Test]
	public void IsVideoFile_VideoFile_True()
	{
		var videoFile = new FileInfo("movie.mp4");
		Assert.IsTrue(_fileTypes.IsVideoFile(videoFile));
	}

	[Test]
	public void IsXmlFile_XmlFile_True()
	{
		var xmlFile = new FileInfo("markup.xml");
		Assert.IsTrue(_fileTypes.IsXmlFile(xmlFile));
	}

	// [Test]
	// public void IsFindableFile_XmlFile_True()
	// {
	// 	var xmlFile = new FileInfo("markup.xml");
	// 	Assert.IsTrue(_fileTypes.IsFindableFile(xmlFile));
	// }
}
