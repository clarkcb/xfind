using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Data;
using System.IO;
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
	private readonly IDictionary<string, FileType> _fileExtTypeIdDictionary;

	public FileTypes()
	{
		_conn = new SqliteConnection("Data Source=" + FindConfig.XfindDb + ";Mode=ReadOnly");
		_fileExtTypeIdDictionary = new ConcurrentDictionary<string, FileType>();
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

	private FileType GetFileTypeForQueryAndElem(string query, string elem)
	{
		var conn = GetConnection();
		using var command = conn.CreateCommand();
		command.CommandText = query;
		command.Parameters.AddWithValue("$x0", elem);

		using var reader = command.ExecuteReader(CommandBehavior.SingleRow);
		if (reader.Read())
		{
			var fileTypeId = reader.GetInt32(0) - 1;
			return (FileType)(fileTypeId);
		}

		return FileType.Unknown;
	}

	private FileType GetFileTypeForFileName(string fileName)
	{
		if (string.IsNullOrEmpty(fileName))
		{
			return FileType.Unknown;
		}

		const string query = "SELECT file_type_id FROM file_name WHERE name = $x0";
		return GetFileTypeForQueryAndElem(query, fileName);
	}

	private FileType GetFileTypeForExtension(string fileExt)
	{
		if (string.IsNullOrEmpty(fileExt))
		{
			return FileType.Unknown;
		}
		if (_fileExtTypeIdDictionary.TryGetValue(fileExt, out var value))
		{
			return value;
		}
		
		const string query = "SELECT file_type_id FROM file_extension WHERE extension = $x0";
		var fileType = GetFileTypeForQueryAndElem(query, fileExt);
		_fileExtTypeIdDictionary[fileExt] = fileType;
		return fileType;
	}

	public FileType GetFileType(FileInfo f)
	{
		var fileTypeForFileName = GetFileTypeForFileName(f.Name);
		if (fileTypeForFileName != FileType.Unknown)
		{
			return fileTypeForFileName;
		}
		return GetFileTypeForExtension(FileUtil.GetFileExtension(f));
	}

	public bool IsArchiveFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Archive;
	}

	public bool IsAudioFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Audio;
	}

	public bool IsBinaryFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Binary;
	}

	public bool IsCodeFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Code;
	}

	public bool IsFontFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Font;
	}

	public bool IsImageFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Image;
	}

	public bool IsTextFile(FileInfo f)
	{
		var fileType = GetFileType(f);
		return fileType is FileType.Text or FileType.Code or FileType.Xml;
	}

	public bool IsVideoFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Video;
	}

	public bool IsUnknownFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Unknown;
	}

	public bool IsXmlFile(FileInfo f)
	{
		return GetFileType(f) == FileType.Xml;
	}
}
