using System.Collections.Generic;
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
	public System.DateTime? MaxLastMod { get; set; }
	public int MaxSize { get; set; }
	public int MinDepth { get; set; }
	public System.DateTime? MinLastMod { get; set; }
	public int MinSize { get; set; }
	public ISet<string> OutArchiveExtensions { get; private set; }
	public ISet<Regex> OutArchiveFilePatterns { get; private set; }
	public ISet<Regex> OutDirPatterns { get; private set; }
	public ISet<string> OutExtensions { get; private set; }
	public ISet<Regex> OutFilePatterns { get; private set; }
	public ISet<FileType> OutFileTypes { get; private set; }
	public ISet<string> Paths { get; private set; }
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
		Debug = false;
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
		Paths = new HashSet<string>();
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

	public FindSettings(
		bool archivesOnly,
		bool debug,
		bool followSymlinks,
		ISet<string> inArchiveExtensions,
		ISet<Regex> inArchiveFilePatterns,
		ISet<Regex> inDirPatterns,
		ISet<string> inExtensions,
		ISet<Regex> inFilePatterns,
		ISet<FileType> inFileTypes,
		bool includeArchives,
		bool includeHidden,
		int maxDepth,
		System.DateTime? maxLastMod,
		int maxSize,
		int minDepth,
		System.DateTime? minLastMod,
		int minSize,
		ISet<string> outArchiveExtensions,
		ISet<Regex> outArchiveFilePatterns,
		ISet<Regex> outDirPatterns,
		ISet<string> outExtensions,
		ISet<Regex> outFilePatterns,
		ISet<FileType> outFileTypes,
		ISet<string> paths,
		bool printDirs,
		bool printFiles,
		bool printUsage,
		bool printVersion,
		bool recursive,
		SortBy sortBy,
		bool sortCaseInsensitive,
		bool sortDescending,
		bool verbose)
	{
		ArchivesOnly = archivesOnly;
		Debug = debug;
		FollowSymlinks = followSymlinks;
		InArchiveExtensions = inArchiveExtensions;
		InArchiveFilePatterns = inArchiveFilePatterns;
		InDirPatterns = inDirPatterns;
		InExtensions = inExtensions;
		InFilePatterns = inFilePatterns;
		InFileTypes = inFileTypes;
		IncludeArchives = includeArchives;
		IncludeHidden = includeHidden;
		PrintDirs = printDirs;
		PrintFiles = printFiles;
		MaxDepth = maxDepth;
		MaxLastMod = maxLastMod;
		MaxSize = maxSize;
		MinDepth = minDepth;
		MinLastMod = minLastMod;
		MinSize = minSize;
		OutArchiveExtensions = outArchiveExtensions;
		OutArchiveFilePatterns = outArchiveFilePatterns;
		OutDirPatterns = outDirPatterns;
		OutExtensions = outExtensions;
		OutFilePatterns = outFilePatterns;
		OutFileTypes = outFileTypes;
		Paths = paths;
		PrintUsage = printUsage;
		PrintVersion = printVersion;
		Recursive = recursive;
		SortBy = sortBy;
		SortCaseInsensitive = sortCaseInsensitive;
		SortDescending = sortDescending;
		Verbose = verbose;
	}

	private static void AddExtension(ISet<string> set, string extList)
	{
		var exts = extList.Split(new[] { ',' });
		foreach (var x in exts)
		{
			var ext = x;
			if (!ext.StartsWith("."))
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
		var typeNames = typeNameList.Split(new[] { ',' });
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

	public void SetSortBy(string sortByName)
	{
		SortBy = SortByUtil.GetSortByFromName(sortByName);
	}

	private static string DateTimeToString(System.DateTime? dt)
	{
		return dt == null ? "0" : $"\"{dt}\"";
	}

	private static string EnumerableToString<T>(IEnumerable<T> enumerable, bool quote = true)
	{
		var sb = new StringBuilder("[");
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
		sb.Append(']');
		return sb.ToString();
	}

	public override string ToString()
	{
		return "FindSettings(" +
		       "ArchivesOnly=" + ArchivesOnly +
		       ", Debug=" + Debug +
		       ", FollowSymlinks=" + FollowSymlinks +
		       ", InArchiveExtensions=" + EnumerableToString(InArchiveExtensions) +
		       ", InArchiveFilePatterns=" + EnumerableToString(InArchiveFilePatterns) +
		       ", InDirPatterns=" + EnumerableToString(InDirPatterns) +
		       ", InExtensions=" + EnumerableToString(InExtensions) +
		       ", InFilePatterns=" + EnumerableToString(InFilePatterns) +
		       ", InFileTypes=" + EnumerableToString(InFileTypes, false) +
		       ", IncludeArchives=" + IncludeArchives +
		       ", IncludeHidden=" + IncludeHidden +
		       ", MaxDepth=" + MaxDepth +
		       ", MaxLastMod=" + DateTimeToString(MaxLastMod) +
		       ", MaxSize=" + MaxSize +
		       ", MinDepth=" + MinDepth +
		       ", MinLastMod=" + DateTimeToString(MinLastMod) +
		       ", MinSize=" + MinSize +
		       ", OutArchiveExtensions=" + EnumerableToString(OutArchiveExtensions) +
		       ", OutArchiveFilePatterns=" + EnumerableToString(OutArchiveFilePatterns) +
		       ", OutDirPatterns=" + EnumerableToString(OutDirPatterns) +
		       ", OutExtensions=" + EnumerableToString(OutExtensions) +
		       ", OutFilePatterns=" + EnumerableToString(OutFilePatterns) +
		       ", OutFileTypes=" + EnumerableToString(OutFileTypes, false) +
		       ", Paths=" + EnumerableToString(Paths) +
		       ", PrintDirs=" + PrintDirs +
		       ", PrintFiles=" + PrintFiles +
		       ", PrintUsage=" + PrintUsage +
		       ", PrintVersion=" + PrintVersion +
		       ", Recursive=" + Recursive +
		       ", SortBy=" + SortByUtil.GetNameFromSortBy(SortBy) +
		       ", SortCaseInsensitive=" + SortCaseInsensitive +
		       ", SortDescending=" + SortDescending +
		       ", Verbose=" + Verbose +
		       ")";
	}
}
