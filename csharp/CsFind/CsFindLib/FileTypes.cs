using System.Collections.Generic;
using System.IO;
using FileTypesDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string, object>>>;

namespace CsFindLib;

public enum FileType
{
	Unknown,
	Archive,
	Binary,
	Code,
	Text,
	Xml
};

public partial class FileTypes
{
	public readonly ISet<string> CurrentAndParentDirs = new HashSet<string> {".", ".."};

	private const string Archive = "archive";
	private const string Binary = "binary";
	private const string Code = "code";
	private const string Text = "text";
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
				Binary => FileType.Binary,
				Code => FileType.Code,
				Text => FileType.Text,
				Xml => FileType.Xml,
				_ => FileType.Unknown
			};
	}

	public FileType GetFileType(FileInfo f)
	{
		if (IsArchiveFile(f)) return FileType.Archive;
		if (IsBinaryFile(f)) return FileType.Binary;
		if (IsCodeFile(f)) return FileType.Code;
		if (IsXmlFile(f)) return FileType.Xml;
		return IsTextFile(f) ? FileType.Text : FileType.Unknown;
	}

	public bool IsArchiveFile(FileInfo f)
	{
		return  _fileTypeNameDictionary[Archive].Contains(f.Name)
		        || _fileTypeExtDictionary[Archive].Contains(f.Extension.ToLowerInvariant());
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

	public bool IsTextFile(FileInfo f)
	{
		return  _fileTypeNameDictionary[Text].Contains(f.Name) ||
		        _fileTypeExtDictionary[Text].Contains(f.Extension.ToLowerInvariant()) ||
		        _fileTypeNameDictionary[Code].Contains(f.Name) ||
		        _fileTypeExtDictionary[Code].Contains(f.Extension.ToLowerInvariant()) ||
		        _fileTypeNameDictionary[Xml].Contains(f.Name) ||
		        _fileTypeExtDictionary[Xml].Contains(f.Extension.ToLowerInvariant());
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
