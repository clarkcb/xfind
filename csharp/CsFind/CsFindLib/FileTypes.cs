using System.Collections.Generic;
using System.IO;

namespace CsFindLib;

public enum FileType
{
	Unknown,
	Archive,
	Audio,
	Binary,
	Code,
	Font,
	Image,
	Text,
	Video,
	Xml
};

public partial class FileTypes
{
	// public readonly ISet<string> CurrentAndParentDirs = new HashSet<string> {".", ".."};

	private const string Archive = "archive";
	private const string Audio = "audio";
	private const string Binary = "binary";
	private const string Code = "code";
	private const string Font = "font";
	private const string Image = "image";
	private const string Text = "text";
	private const string Video = "video";
	private const string Xml = "xml";

	private readonly IDictionary<string, ISet<string>> _fileTypeExtDictionary;
	private readonly IDictionary<string, ISet<string>> _fileTypeNameDictionary;

	public FileTypes()
	{
		_fileTypeExtDictionary = new Dictionary<string, ISet<string>>();
		_fileTypeNameDictionary = new Dictionary<string, ISet<string>>();
		PopulateFileTypes();
	}

	partial void PopulateFileTypes();

	public static FileType FromName(string name)
	{
		return string.IsNullOrEmpty(name)
			? FileType.Unknown
			: name.ToLowerInvariant() switch
			{
				Archive => FileType.Archive,
				Audio => FileType.Audio,
				Binary => FileType.Binary,
				Code => FileType.Code,
				Font => FileType.Font,
				Image => FileType.Image,
				Text => FileType.Text,
				Video => FileType.Video,
				Xml => FileType.Xml,
				_ => FileType.Unknown
			};
	}

	public FileType GetFileType(FileInfo f)
	{
		// more specific first
		if (IsCodeFile(f)) return FileType.Code;
		if (IsArchiveFile(f)) return FileType.Archive;
		if (IsAudioFile(f)) return FileType.Audio;
		if (IsFontFile(f)) return FileType.Font;
		if (IsImageFile(f)) return FileType.Image;
		if (IsVideoFile(f)) return FileType.Video;
		// more general last
		if (IsXmlFile(f)) return FileType.Xml;
		if (IsTextFile(f)) return FileType.Text;
		if (IsBinaryFile(f)) return FileType.Binary;
		return FileType.Unknown;
	}

	public bool IsArchiveFile(FileInfo f)
	{
		return  _fileTypeNameDictionary[Archive].Contains(f.Name)
		        || _fileTypeExtDictionary[Archive].Contains(f.Extension.ToLowerInvariant());
	}

	public bool IsAudioFile(FileInfo f)
	{
		return  _fileTypeNameDictionary[Audio].Contains(f.Name)
		        || _fileTypeExtDictionary[Audio].Contains(f.Extension.ToLowerInvariant());
	}

	public bool IsBinaryFile(FileInfo f)
	{
		return  _fileTypeNameDictionary[Binary].Contains(f.Name)
		        || _fileTypeExtDictionary[Binary].Contains(f.Extension.ToLowerInvariant());
	}

	public bool IsCodeFile(FileInfo f)
	{
		return _fileTypeNameDictionary[Code].Contains(f.Name)
		       || _fileTypeExtDictionary[Code].Contains(f.Extension.ToLowerInvariant());
	}

	public bool IsFontFile(FileInfo f)
	{
		return _fileTypeNameDictionary[Font].Contains(f.Name)
		       || _fileTypeExtDictionary[Font].Contains(f.Extension.ToLowerInvariant());
	}

	public bool IsImageFile(FileInfo f)
	{
		return _fileTypeNameDictionary[Image].Contains(f.Name)
		       || _fileTypeExtDictionary[Image].Contains(f.Extension.ToLowerInvariant());
	}

	public bool IsTextFile(FileInfo f)
	{
		return  _fileTypeNameDictionary[Text].Contains(f.Name) ||
		        _fileTypeExtDictionary[Text].Contains(f.Extension.ToLowerInvariant()) ||
		        _fileTypeNameDictionary[Code].Contains(f.Name) ||
		        _fileTypeExtDictionary[Code].Contains(f.Extension.ToLowerInvariant()) ||
		        _fileTypeNameDictionary[Xml].Contains(f.Name) ||
		        _fileTypeExtDictionary[Xml].Contains(f.Extension.ToLowerInvariant());
	}

	public bool IsVideoFile(FileInfo f)
	{
		return _fileTypeNameDictionary[Video].Contains(f.Name)
		       || _fileTypeExtDictionary[Video].Contains(f.Extension.ToLowerInvariant());
	}

	public bool IsUnknownFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Unknown;
	}

	public bool IsXmlFile(FileInfo f)
	{
		return  _fileTypeNameDictionary[Xml].Contains(f.Name)
		        || _fileTypeExtDictionary[Xml].Contains(f.Extension.ToLowerInvariant());
	}
}
