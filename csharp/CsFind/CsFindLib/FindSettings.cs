using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;

namespace CsFindLib;

public class FindSettings
{
	private bool _archivesOnly;

	public bool ArchivesOnly
	{
		get => _archivesOnly;
		set
		{
			_archivesOnly = value;
			if (_archivesOnly)
				IncludeArchives = true;
		}
	}

	public bool Colorize { get; set; }

	private bool _debug;
	public bool Debug
	{
		get => _debug;
		set
		{
			_debug = value;
			if (_debug)
				Verbose = true;
		}
	}

	public bool DefaultFiles { get; set; }
	public Color DirColor { get; set; }
	public Color ExtColor { get; set; }
	public Color FileColor { get; set; }
	public bool FollowSymlinks { get; set; }
	public ISet<string> InArchiveExtensions { get; private set; }
	public ISet<Regex> InArchiveFilePatterns { get; private set; }
	public ISet<Regex> InDirPatterns { get; private set; }
	public ISet<string> InExtensions { get; private set; }
	public ISet<Regex> InFilePatterns { get; private set; }
	public ISet<FileType> InFileTypes { get; private set; }
	public bool IncludeArchives { get; set; }
	public bool IncludeHidden { get; set; }
	public int MaxDepth { get; set; }
	public DateTime? MaxLastMod { get; set; }
	public int MaxSize { get; set; }
	public int MinDepth { get; set; }
	public DateTime? MinLastMod { get; set; }
	public int MinSize { get; set; }
	public ISet<string> OutArchiveExtensions { get; private set; }
	public ISet<Regex> OutArchiveFilePatterns { get; private set; }
	public ISet<Regex> OutDirPatterns { get; private set; }
	public ISet<string> OutExtensions { get; private set; }
	public ISet<Regex> OutFilePatterns { get; private set; }
	public ISet<FileType> OutFileTypes { get; private set; }
	public ISet<FilePath> Paths { get; private set; }
	public bool PrintDirs { get; set; }
	public bool PrintFiles { get; set; }
	public bool PrintUsage { get; set; }
	public bool PrintVersion { get; set; }
	public bool Recursive { get; set; }
	public SortBy SortBy { get; set; }
	public bool SortCaseInsensitive { get; set; }
	public bool SortDescending { get; set; }
	public bool Verbose { get; set; }

	public FindSettings()
	{
		ArchivesOnly = false;
		Colorize = true;
		Debug = false;
		DefaultFiles = true;
		DirColor = Color.Cyan;
		ExtColor = Color.Yellow;
		FileColor = Color.Magenta;
		FollowSymlinks = false;
		InArchiveExtensions = new HashSet<string>();
		InArchiveFilePatterns = new HashSet<Regex>();
		InDirPatterns = new HashSet<Regex>();
		InExtensions = new HashSet<string>();
		InFilePatterns = new HashSet<Regex>();
		InFileTypes = new HashSet<FileType>();
		IncludeArchives = false;
		IncludeHidden = false;
		MaxDepth = -1;
		MaxLastMod = null;
		MaxSize = 0;
		MinDepth = -1;
		MinLastMod = null;
		MinSize = 0;
		OutArchiveExtensions = new HashSet<string>();
		OutArchiveFilePatterns = new HashSet<Regex>();
		OutDirPatterns = new HashSet<Regex>();
		OutExtensions = new HashSet<string>();
		OutFilePatterns = new HashSet<Regex>();
		OutFileTypes = new HashSet<FileType>();
		Paths = new HashSet<FilePath>(new FilePathComparer());
		PrintDirs = false;
		PrintFiles = false;
		PrintUsage = false;
		PrintVersion = false;
		Recursive = true;
		SortBy = SortBy.FilePath;
		SortCaseInsensitive = false;
		SortDescending = false;
		Verbose = false;
	}

	private static void AddExtension(ISet<string> set, string extList)
	{
		var exts = extList.Split([',']);
		foreach (var x in exts)
		{
			var ext = x;
			if (!ext.StartsWith('.'))
				ext = "." + ext;
			set.Add(ext.ToLowerInvariant());
		}
	}

	public void AddInExtension(string ext)
	{
		AddExtension(InExtensions, ext);
	}

	public void AddOutExtension(string ext)
	{
		AddExtension(OutExtensions, ext);
	}

	private static void AddPattern(ISet<Regex> set, string pattern)
	{
		set.Add(new Regex(pattern, RegexOptions.Compiled));
	}

	public void AddInDirPattern(string pattern)
	{
		AddPattern(InDirPatterns, pattern);
	}

	public void AddOutDirPattern(string pattern)
	{
		AddPattern(OutDirPatterns, pattern);
	}

	public void AddInFilePattern(string pattern)
	{
		AddPattern(InFilePatterns, pattern);
	}

	public void AddOutFilePattern(string pattern)
	{
		AddPattern(OutFilePatterns, pattern);
	}

	public void AddInArchiveExtension(string ext)
	{
		AddExtension(InArchiveExtensions, ext);
	}

	public void AddOutArchiveExtension(string ext)
	{
		AddExtension(OutArchiveExtensions, ext);
	}

	public void AddInArchiveFilePattern(string pattern)
	{
		AddPattern(InArchiveFilePatterns, pattern);
	}

	public void AddOutArchiveFilePattern(string pattern)
	{
		AddPattern(OutArchiveFilePatterns, pattern);
	}

	private static void AddFileType(ISet<FileType> set, string typeNameList)
	{
		var typeNames = typeNameList.Split([',']);
		foreach (var t in typeNames)
		{
			set.Add(FileTypes.FromName(t));
		}
	}

	public void AddInFileType(string typeName)
	{
		AddFileType(InFileTypes, typeName);
	}

	public void AddOutFileType(string typeName)
	{
		AddFileType(OutFileTypes, typeName);
	}

	public void AddPath(string path)
	{
		Paths.Add(new FilePath(path));
	}

	public void SetSortBy(string sortByName)
	{
		SortBy = SortByUtil.GetSortByFromName(sortByName);
	}

	private static string DateTimeToString(DateTime? dt)
	{
		return dt == null ? "0" : $"\"{dt}\"";
	}

	private static string EnumerableToString<T>(IEnumerable<T>? enumerable, bool quote = true)
	{
		var sb = new StringBuilder("[");
		if (enumerable != null)
		{
			var elemCount = 0;
			foreach (var x in enumerable)
			{
				if (elemCount > 0)
					sb.Append(", ");
				if (quote)
					sb.Append('"');
				sb.Append(x);
				if (quote)
					sb.Append('"');
				elemCount++;
			}
		}
		sb.Append(']');
		return sb.ToString();
	}

	public override string ToString()
	{
		var sb = new StringBuilder(GetType().Name);
		sb.Append('(');
		var flags = BindingFlags.Public | BindingFlags.Instance;
        var classType = GetType();
		var properties = classType.GetProperties(flags).OrderBy(p => p.Name).ToArray();
		var propCount = 0;
		foreach (var p in properties)
		{
			if (p.PropertyType.FullName == null) continue;
			if (propCount > 0)
			{
				sb.Append(", ");
			}
			sb.Append(p.Name).Append("=");
			if (p.PropertyType.FullName.StartsWith("System.Collections.Generic.ISet"))
			{
				if (p.PropertyType.IsGenericType && p.PropertyType.GenericTypeArguments.Length == 1)
				{
					if (p.PropertyType.GenericTypeArguments[0] == typeof(string))
					{
						sb.Append(EnumerableToString(p.GetValue(this, null)  as ISet<string>));
					}
					else if (p.PropertyType.GenericTypeArguments[0] == typeof(FileType))
					{
						sb.Append(EnumerableToString(p.GetValue(this, null)  as ISet<FileType>, false));
					}
					else if (p.PropertyType.GenericTypeArguments[0] == typeof(Regex))
					{
						sb.Append(EnumerableToString(p.GetValue(this, null)  as ISet<Regex>));
					}
					else if (p.PropertyType.GenericTypeArguments[0] == typeof(FilePath))
					{
						sb.Append(EnumerableToString(p.GetValue(this, null)  as ISet<FilePath>));
					}
				}
			}
			else if (p.PropertyType.FullName.StartsWith("System.Nullable"))
			{
				if (p.PropertyType.IsGenericType && p.PropertyType.GenericTypeArguments.Length == 1)
				{
					if (p.PropertyType.GenericTypeArguments[0] == typeof(DateTime))
					{
						sb.Append(DateTimeToString(p.GetValue(this, null) as DateTime?));
					}
				}
			}
			else if (p.PropertyType.FullName.Equals("CsFindLib.SortBy"))
			{
				sb.Append(SortByUtil.GetNameFromSortBy(p.GetValue(this) as SortBy?));
			}
			else
			{
				sb.Append(p.GetValue(this)?.ToString() ?? "null");
			}
			propCount++;
		}

		sb.Append(')');
		return sb.ToString();
	}
}
