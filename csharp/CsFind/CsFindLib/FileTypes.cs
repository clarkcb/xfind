using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using Microsoft.Data.Sqlite;

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
	
	private SqliteConnection _conn;
	private readonly IDictionary<string, FileType> _extTypeCache;
	private readonly IDictionary<string, FileType> _nameTypeCache;
	private bool _nameTypeCacheLoaded;

	public FileTypes()
	{
		_conn = new SqliteConnection("Data Source=" + FindConfig.XfindDb + ";Mode=ReadOnly");
		_extTypeCache = new ConcurrentDictionary<string, FileType>();
		_nameTypeCache = new ConcurrentDictionary<string, FileType>();
	}

	private SqliteConnection GetConnection()
	{
		if (_conn.State == ConnectionState.Closed)
		{
			_conn.Open();
		}
		return _conn;
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

	private IDictionary<string, FileType> GetFileTypesForQueryAndParams(string query, List<string> param)
	{
		var conn = GetConnection();
		using var command = conn.CreateCommand();
		command.CommandText = query;
		for (var i = 0; i < param.Count; i++)
		{
			command.Parameters.AddWithValue($"$x{i}", param);
		}
		var results = new Dictionary<string, FileType>();
		using var reader = command.ExecuteReader(CommandBehavior.Default);
		while (reader.Read())
		{
			var key = reader.GetString(0);
			var fileTypeId = reader.GetInt32(1) - 1;
			results[key] = (FileType)fileTypeId;
		}

		return results;
	}

	private void LoadNameTypeCache()
	{
		const string query = "SELECT name, file_type_id FROM file_name";
		var results = GetFileTypesForQueryAndParams(query, []);
		foreach (var kv in results)
		{
			var name = kv.Key;
			var fileTypeId = kv.Value;
			_nameTypeCache[name] = (FileType)fileTypeId;
		}
		_nameTypeCacheLoaded = true;
	}

	private FileType GetFileTypeForQueryAndParams(string query, List<string> param)
	{
		var conn = GetConnection();
		using var command = conn.CreateCommand();
		command.CommandText = query;
		for (var i = 0; i < param.Count; i++)
		{
			command.Parameters.AddWithValue($"$x{i}", param[i]);
		}
		using var reader = command.ExecuteReader(CommandBehavior.SingleRow);
		if (reader.Read())
		{
			var fileTypeId = reader.GetInt32(0) - 1;
			return (FileType)fileTypeId;
		}

		return FileType.Unknown;
	}

	private FileType GetFileTypeForFileName(string fileName)
	{
		if (string.IsNullOrEmpty(fileName))
		{
			return FileType.Unknown;
		}
		if (!_nameTypeCacheLoaded)
		{
			LoadNameTypeCache();
		}
		if (_nameTypeCache.TryGetValue(fileName, out var value))
		{
			return value;
		}
		// const string query = "SELECT file_type_id FROM file_name WHERE name = $x0";
		// return GetFileTypeForQueryAndParams(query, [fileName]);
		return FileType.Unknown;
	}

	private FileType GetFileTypeForExtension(string fileExt)
	{
		if (string.IsNullOrEmpty(fileExt))
		{
			return FileType.Unknown;
		}
		if (_extTypeCache.TryGetValue(fileExt, out var value))
		{
			return value;
		}
		
		const string query = "SELECT file_type_id FROM file_extension WHERE extension = $x0";
		var fileType = GetFileTypeForQueryAndParams(query, [fileExt]);
		_extTypeCache[fileExt] = fileType;
		return fileType;
	}

	public FileType GetFileType(FilePath filePath)
	{
		var fileTypeForFileName = GetFileTypeForFileName(filePath.Name);
		if (fileTypeForFileName != FileType.Unknown)
		{
			return fileTypeForFileName;
		}
		return GetFileTypeForExtension(FileUtil.GetFileExtension(filePath));
	}

	public bool IsArchiveFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Archive;
	}

	public bool IsAudioFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Audio;
	}

	public bool IsBinaryFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Binary;
	}

	public bool IsCodeFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Code;
	}

	public bool IsFontFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Font;
	}

	public bool IsImageFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Image;
	}

	public bool IsTextFile(FilePath filePath)
	{
		var fileType = GetFileType(filePath);
		return fileType is FileType.Text or FileType.Code or FileType.Xml;
	}

	public bool IsVideoFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Video;
	}

	public bool IsUnknownFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Unknown;
	}

	public bool IsXmlFile(FilePath filePath)
	{
		return GetFileType(filePath) == FileType.Xml;
	}
}
