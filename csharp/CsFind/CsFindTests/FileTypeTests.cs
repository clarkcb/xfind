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
		Assert.That(_fileTypes.GetFileType(archiveFile), Is.EqualTo(FileType.Archive));
	}

	[Test]
	public void GetFileType_AudioFile_FileTypeAudio()
	{
		var audioFile = new FileInfo("music.mp3");
		Assert.That(_fileTypes.GetFileType(audioFile), Is.EqualTo(FileType.Audio));
	}

	[Test]
	public void GetFileType_BinaryFile_FileTypeBinary()
	{
		var binaryFile = new FileInfo("binary.exe");
		Assert.That(_fileTypes.GetFileType(binaryFile), Is.EqualTo(FileType.Binary));
	}

	[Test]
	public void GetFileType_CodeFile_FileTypeCode()
	{
		var codeFile = new FileInfo("code.cs");
		Assert.That(_fileTypes.GetFileType(codeFile), Is.EqualTo(FileType.Code));
	}

	[Test]
	public void GetFileType_FontFile_FileTypeCode()
	{
		var fontFile = new FileInfo("font.ttf");
		Assert.That(_fileTypes.GetFileType(fontFile), Is.EqualTo(FileType.Font));
	}

	[Test]
	public void GetFileType_ImageFile_FileTypeImage()
	{
		var imageFile = new FileInfo("image.png");
		Assert.That(_fileTypes.GetFileType(imageFile), Is.EqualTo(FileType.Image));
	}

	[Test]
	public void GetFileType_TextFile_FileTypeText()
	{
		var textFile = new FileInfo("text.txt");
		Assert.That(_fileTypes.GetFileType(textFile), Is.EqualTo(FileType.Text));
	}

	[Test]
	public void GetFileType_VideoFile_FileTypeVideo()
	{
		var videoFile = new FileInfo("movie.mp4");
		Assert.That(_fileTypes.GetFileType(videoFile), Is.EqualTo(FileType.Video));
	}

	[Test]
	public void GetFileType_XmlFile_FileTypeXml()
	{
		var xmlFile = new FileInfo("markup.xml");
		Assert.That(_fileTypes.GetFileType(xmlFile), Is.EqualTo(FileType.Xml));
	}

	[Test]
	public void GetFileType_UnknownFile_FileTypeUnknown()
	{
		var unknownFile = new FileInfo("unknown.xyz");
		Assert.That(_fileTypes.GetFileType(unknownFile), Is.EqualTo(FileType.Unknown));
	}

	[Test]
	public void IsArchiveFile_ArchiveFile_True()
	{
		var archiveFile = new FileInfo("archive.zip");
		Assert.That(_fileTypes.IsArchiveFile(archiveFile));
	}

	[Test]
	public void IsAudioFile_AudioFile_True()
	{
		var audioFile = new FileInfo("music.mp3");
		Assert.That(_fileTypes.IsAudioFile(audioFile));
	}

	[Test]
	public void IsBinaryFile_BinaryFile_True()
	{
		var binaryFile = new FileInfo("binary.exe");
		Assert.That(_fileTypes.IsBinaryFile(binaryFile));
	}

	[Test]
	public void IsCodeFile_CodeFile_True()
	{
		var codeFile = new FileInfo("code.cs");
		Assert.That(_fileTypes.IsCodeFile(codeFile));
	}

	[Test]
	public void IsFontFile_FontFile_True()
	{
		var fontFile = new FileInfo("font.ttf");
		Assert.That(_fileTypes.IsFontFile(fontFile));
	}

	[Test]
	public void IsTextFile_ImageFile_True()
	{
		var imageFile = new FileInfo("image.png");
		Assert.That(_fileTypes.IsImageFile(imageFile));
	}

	[Test]
	public void IsTextFile_TextFile_True()
	{
		var textFile = new FileInfo("text.txt");
		Assert.That(_fileTypes.IsTextFile(textFile));
	}

	[Test]
	public void IsVideoFile_VideoFile_True()
	{
		var videoFile = new FileInfo("movie.mp4");
		Assert.That(_fileTypes.IsVideoFile(videoFile));
	}

	[Test]
	public void IsXmlFile_XmlFile_True()
	{
		var xmlFile = new FileInfo("markup.xml");
		Assert.That(_fileTypes.IsXmlFile(xmlFile));
	}

	// [Test]
	// public void IsFindableFile_XmlFile_True()
	// {
	// 	var xmlFile = new FileInfo("markup.xml");
	// 	Assert.That(_fileTypes.IsFindableFile(xmlFile));
	// }
}
