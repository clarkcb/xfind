using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using FileTypesDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string, object>>>;

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

public class FileTypes
{
	private const string Archive = "archive";
	private const string Audio = "audio";
	private const string Binary = "binary";
	private const string Code = "code";
	private const string Font = "font";
	private const string Image = "image";
	private const string Text = "text";
	private const string Video = "video";
	private const string Xml = "xml";

	private readonly string _fileTypesResource;
	private readonly IDictionary<string, ISet<string>> _fileTypeExtDictionary;
	private readonly IDictionary<string, ISet<string>> _fileTypeNameDictionary;

	public FileTypes()
	{
		_fileTypesResource = EmbeddedResource.GetResourceFileContents("CsFindLib.Resources.filetypes.json");
		_fileTypeExtDictionary = new Dictionary<string, ISet<string>>();
		_fileTypeNameDictionary = new Dictionary<string, ISet<string>>();
		PopulateFileTypesFromJson();
	}

	private void PopulateFileTypesFromJson()
	{
		var filetypesDict = JsonSerializer.Deserialize<FileTypesDictionary>(_fileTypesResource);
		if (filetypesDict!.ContainsKey("filetypes"))
		{
			var filetypeDicts = filetypesDict["filetypes"];
			foreach (var filetypeDict in filetypeDicts)
			{
				if (filetypeDict.TryGetValue("type", out var typeValue))
				{
					var name = ((JsonElement)typeValue).GetString();
					if (filetypeDict.TryGetValue("extensions", out var extensionsValue))
					{
						var extensions = ((JsonElement)extensionsValue).EnumerateArray()
							.Select(x => "." + x.GetString());
						var extensionSet = new HashSet<string>(extensions);
						_fileTypeExtDictionary[name!] = extensionSet;
					}
					if (filetypeDict.TryGetValue("names", out var namesValue))
					{
						var names = ((JsonElement)namesValue).EnumerateArray()
							.Select(x => "" + x.GetString());
						var nameSet = new HashSet<string>(names);
						_fileTypeNameDictionary[name!] = nameSet;
					}
				}
			}
		}
	}

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

	public FileType GetFileType(FilePath filePath)
	{
		// more specific first
		if (IsCodeFile(filePath)) return FileType.Code;
		if (IsArchiveFile(filePath)) return FileType.Archive;
		if (IsAudioFile(filePath)) return FileType.Audio;
		if (IsFontFile(filePath)) return FileType.Font;
		if (IsImageFile(filePath)) return FileType.Image;
		if (IsVideoFile(filePath)) return FileType.Video;
		// more general last
		if (IsXmlFile(filePath)) return FileType.Xml;
		if (IsTextFile(filePath)) return FileType.Text;
		if (IsBinaryFile(filePath)) return FileType.Binary;
		return FileType.Unknown;
	}

	public bool IsArchiveFile(FilePath filePath)
	{
		return  _fileTypeNameDictionary[Archive].Contains(filePath.Name)
		        || _fileTypeExtDictionary[Archive].Contains(filePath.Extension.ToLowerInvariant());
	}

	public bool IsAudioFile(FilePath filePath)
	{
		return  _fileTypeNameDictionary[Audio].Contains(filePath.Name)
		        || _fileTypeExtDictionary[Audio].Contains(filePath.Extension.ToLowerInvariant());
	}

	public bool IsBinaryFile(FilePath filePath)
	{
		return  _fileTypeNameDictionary[Binary].Contains(filePath.Name)
		        || _fileTypeExtDictionary[Binary].Contains(filePath.Extension.ToLowerInvariant());
	}

	public bool IsCodeFile(FilePath filePath)
	{
		return _fileTypeNameDictionary[Code].Contains(filePath.Name)
		       || _fileTypeExtDictionary[Code].Contains(filePath.Extension.ToLowerInvariant());
	}

	public bool IsFontFile(FilePath filePath)
	{
		return _fileTypeNameDictionary[Font].Contains(filePath.Name)
		       || _fileTypeExtDictionary[Font].Contains(filePath.Extension.ToLowerInvariant());
	}

	public bool IsImageFile(FilePath filePath)
	{
		return _fileTypeNameDictionary[Image].Contains(filePath.Name)
		       || _fileTypeExtDictionary[Image].Contains(filePath.Extension.ToLowerInvariant());
	}

	public bool IsTextFile(FilePath filePath)
	{
		return  _fileTypeNameDictionary[Text].Contains(filePath.Name) ||
		        _fileTypeExtDictionary[Text].Contains(filePath.Extension.ToLowerInvariant()) ||
		        _fileTypeNameDictionary[Code].Contains(filePath.Name) ||
		        _fileTypeExtDictionary[Code].Contains(filePath.Extension.ToLowerInvariant()) ||
		        _fileTypeNameDictionary[Xml].Contains(filePath.Name) ||
		        _fileTypeExtDictionary[Xml].Contains(filePath.Extension.ToLowerInvariant());
	}

	public bool IsVideoFile(FilePath filePath)
	{
		return _fileTypeNameDictionary[Video].Contains(filePath.Name)
		       || _fileTypeExtDictionary[Video].Contains(filePath.Extension.ToLowerInvariant());
	}

	public bool IsUnknownFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Unknown;
	}

	public bool IsXmlFile(FilePath filePath)
	{
		return  _fileTypeNameDictionary[Xml].Contains(filePath.Name)
		        || _fileTypeExtDictionary[Xml].Contains(filePath.Extension.ToLowerInvariant());
	}
}
