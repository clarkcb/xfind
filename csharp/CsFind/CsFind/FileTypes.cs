using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Xml.Linq;

using FileTypesDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string,object>>>;

namespace CsFind
{
	public enum FileType
	{
		Unknown,
		Archive,
		Binary,
		Code,
		Text,
		Xml
	};

	public class FileTypes
	{
		public readonly ISet<string> CurrentAndParentDirs = new HashSet<string> {".", ".."};

		private const string Archive = "archive";
		private const string Binary = "binary";
		private const string Code = "code";
		private const string Text = "text";
		private const string Xml = "xml";

		private readonly string _fileTypesResource;
		private readonly IDictionary<string, ISet<string>> _fileTypesDictionary;

		public FileTypes()
		{
			_fileTypesResource = EmbeddedResource.GetResourceFileContents("CsFind.Resources.filetypes.json");
			_fileTypesDictionary = new Dictionary<string, ISet<string>>();
			PopulateFileTypesFromJson();
		}

		private void PopulateFileTypesFromJson()
		{
			var filetypesDict = JsonSerializer.Deserialize<FileTypesDictionary>(_fileTypesResource);
			if (filetypesDict.ContainsKey("filetypes"))
			{
				var filetypeDicts = filetypesDict["filetypes"];
				foreach (var filetypeDict in filetypeDicts)
				{
					if (filetypeDict.ContainsKey("type") && filetypeDict.ContainsKey("extensions"))
					{
						var name = ((JsonElement)filetypeDict["type"]).GetString();
						var extensions = ((JsonElement)filetypeDict["extensions"]).EnumerateArray()
							.Select(x => "." + x.GetString());
						var extensionSet = new HashSet<string>(extensions);
						_fileTypesDictionary[name] = extensionSet;
					}
				}
				_fileTypesDictionary[Text].UnionWith(_fileTypesDictionary[Code]);
				_fileTypesDictionary[Text].UnionWith(_fileTypesDictionary[Xml]);
			}
		}

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
			return _fileTypesDictionary[Archive].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsBinaryFile(FileInfo f)
		{
			return _fileTypesDictionary[Binary].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsCodeFile(FileInfo f)
		{
			return _fileTypesDictionary[Code].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsTextFile(FileInfo f)
		{
			return _fileTypesDictionary[Text].Contains(f.Extension.ToLowerInvariant());
		}

		public bool IsUnknownFile(FileInfo f)
		{
			return GetFileType(f) == FileType.Unknown;
		}

		public bool IsXmlFile(FileInfo f)
		{
			return _fileTypesDictionary[Xml].Contains(f.Extension.ToLowerInvariant());
		}
	}
}
