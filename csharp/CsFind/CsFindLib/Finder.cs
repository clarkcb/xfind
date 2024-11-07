using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace CsFindLib;

public class Finder
{
	private readonly FileTypes _fileTypes;
	private readonly EnumerationOptions _enumerationOptions;
	private FindSettings Settings { get; }
	public Finder(FindSettings settings)
	{
		Settings = settings;
		ValidateSettings();
		_fileTypes = new FileTypes();
		_enumerationOptions = GetEnumerationOptionsForSettings();
	}

	private void ValidateSettings()
	{
		if (Settings.Paths.Count == 0)
			throw new FindException("Startpath not defined");
		if (Settings.Paths.Select(FileUtil.ExpandPath).Any(p => !Path.Exists(p)))
		{
			throw new FindException("Startpath not found");
		}
		if (Settings is { MaxDepth: > -1, MinDepth: > -1 } && Settings.MaxDepth < Settings.MinDepth)
		{
			throw new FindException("Invalid range for mindepth and maxdepth");
		}
		if (Settings is { MaxLastMod: not null, MinLastMod: not null }
		    && Settings.MaxLastMod < Settings.MinLastMod)
		{
			throw new FindException("Invalid range for minlastmod and maxlastmod");
		}
		if (Settings is { MaxSize: > 0, MinSize: > 0 } && Settings.MaxSize < Settings.MinSize)
		{
			throw new FindException("Invalid range for minsize and maxsize");
		}
	}

	private EnumerationOptions GetEnumerationOptionsForSettings()
	{
		var enumerationOptions = new EnumerationOptions
		{
			AttributesToSkip = FileAttributes.System,
			IgnoreInaccessible = true, // TODO: maybe we want to know when a file isn't accessible?
			MatchType = MatchType.Simple,
			// Set recursion to false because we will do it manually
			RecurseSubdirectories = false,
			ReturnSpecialDirectories = false
		};
		if (!Settings.IncludeArchives)
		{
			enumerationOptions.AttributesToSkip |= FileAttributes.Compressed;
		}
		if (!Settings.IncludeHidden)
		{
			enumerationOptions.AttributesToSkip |= FileAttributes.Hidden;
		}

		return enumerationOptions;
	}

	private bool MatchesAnyPattern(string s, ISet<Regex> patterns)
	{
		return patterns.Any(p => p.Matches(s).Count > 0);
	}

	private bool AnyMatchesAnyPattern(IEnumerable<string> slist, ISet<Regex> patterns)
	{
		return slist.Any(s => MatchesAnyPattern(s, patterns));
	}
	
	public bool IsMatchingDirectory(DirectoryInfo? d)
	{
		if (d == null)
		{
			return true;
		}

		// This is how we detect and filter out symlinked directories
		if (!Settings.FollowSymlinks && d.Exists && d.Attributes.HasFlag(FileAttributes.ReparsePoint))
		{
			return false;
		}

		var elems = FileUtil.GetDirElems(d).ToList();

		if (!Settings.IncludeHidden)
		{
			if (elems.Any(FileUtil.IsHidden))
			{
				return false;
			}
		}

		return (Settings.InDirPatterns.Count == 0 ||
		        AnyMatchesAnyPattern(elems, Settings.InDirPatterns)) &&
		       (Settings.OutDirPatterns.Count == 0 ||
		        !AnyMatchesAnyPattern(elems, Settings.OutDirPatterns));
	}

	public bool IsMatchingArchiveFileExtension(string ext)
	{
		return (Settings.InArchiveExtensions.Count == 0 ||
		         Settings.InArchiveExtensions.Contains(ext)) &&
		        (Settings.OutArchiveExtensions.Count == 0 ||
		         !Settings.OutArchiveExtensions.Contains(ext));
	}

	public bool IsMatchingFileExtension(string ext)
	{
		return (Settings.InExtensions.Count == 0 ||
		         Settings.InExtensions.Contains(ext)) &&
		        (Settings.OutExtensions.Count == 0 ||
		         !Settings.OutExtensions.Contains(ext));
	}

	public bool IsMatchingArchiveFileName(string fileName)
	{
		return (Settings.InArchiveFilePatterns.Count == 0 ||
		        Settings.InArchiveFilePatterns.Any(p => p.Match(fileName).Success)) &&
		       (Settings.OutArchiveFilePatterns.Count == 0 ||
		        !Settings.OutArchiveFilePatterns.Any(p => p.Match(fileName).Success));
	}

	public bool IsMatchingFileName(string fileName)
	{
		return (Settings.InFilePatterns.Count == 0 ||
		        Settings.InFilePatterns.Any(p => p.Match(fileName).Success)) &&
		       (Settings.OutFilePatterns.Count == 0 ||
		        !Settings.OutFilePatterns.Any(p => p.Match(fileName).Success));
	}

	public bool IsMatchingFileType(FileType fileType)
	{
		return (Settings.InFileTypes.Count == 0 ||
		        Settings.InFileTypes.Contains(fileType)) &&
		       (Settings.OutFileTypes.Count == 0 ||
		        !Settings.OutFileTypes.Contains(fileType));
	}

	public bool IsMatchingFileSize(long fileSize)
	{
		return (Settings.MaxSize == 0 || fileSize <= Settings.MaxSize) &&
		       (Settings.MinSize == 0 || fileSize >= Settings.MinSize);
	}

	public bool IsMatchingLastMod(DateTime lastMod)
	{
		return (Settings.MaxLastMod == null || lastMod <= Settings.MaxLastMod) &&
		       (Settings.MinLastMod == null || lastMod >= Settings.MinLastMod);
	}

	public bool IsMatchingFileResult(FileResult fr)
	{
		return IsMatchingFileExtension(fr.File.Extension) &&
		       IsMatchingFileName(fr.File.Name) &&
		       IsMatchingFileType(fr.Type) &&
		       IsMatchingFileSize(fr.File.Length) &&
		       IsMatchingLastMod(fr.File.LastWriteTimeUtc);
	}

	public bool IsMatchingArchiveFileResult(FileResult fr)
	{
		return IsMatchingArchiveFileExtension(fr.File.Extension) &&
		       IsMatchingArchiveFileName(fr.File.Name);
	}

	public FileResult? FilterToFileResult(FileInfo fi)
	{
		// This is how we check for / filter out symlinked files
		if (!Settings.FollowSymlinks && fi.Exists && fi.Attributes.HasFlag(FileAttributes.ReparsePoint))
			return null;
		if (!Settings.IncludeHidden && FileUtil.IsHiddenFile(fi))
			return null;
		var fr = new FileResult(fi, _fileTypes.GetFileType(fi));
		if (fr.Type.Equals(FileType.Archive))
		{
			if (Settings.IncludeArchives && IsMatchingArchiveFileResult(fr))
			{
				return fr;
			}

			return null;
		}
		if (!Settings.ArchivesOnly && IsMatchingFileResult(fr))
		{
			return fr;
		}

		return null;
	}

	private IEnumerable<FileResult> RecGetFileResults(DirectoryInfo dir, int minDepth, int maxDepth, int currentDepth)
	{
		var pathResults = new List<FileResult>();
		var recurse = true;
		if (currentDepth == maxDepth)
		{
			recurse = false;
		}
		else if (maxDepth > -1 && currentDepth > maxDepth)
		{
			return pathResults;
		}

		if (minDepth < 0 || currentDepth >= minDepth)
		{
			pathResults.AddRange(dir.EnumerateFiles("*", _enumerationOptions)
				.Select(FilterToFileResult)
				.Where(fr => fr != null)
				.Select(fr => fr!));
		}

		if (recurse)
		{
			var pathDirs = dir.EnumerateDirectories().Where(IsMatchingDirectory);
			foreach (var pathDir in pathDirs)
			{
				pathResults.AddRange(RecGetFileResults(pathDir, minDepth, maxDepth, currentDepth + 1));
			}
		}

		return pathResults;
	}

	private IEnumerable<FileResult> GetFileResults(string filePath)
	{
		if (Directory.Exists(filePath))
		{
			// if MaxDepth is zero, we can skip since a directory cannot be a result
			if (Settings.MaxDepth == 0)
			{
				return [];
			}
			var dir = new DirectoryInfo(filePath);
			if (IsMatchingDirectory(dir))
			{
				var maxDepth = Settings.Recursive ? Settings.MaxDepth : 1;
				return RecGetFileResults(dir, Settings.MinDepth, maxDepth, 1);
			}
		}
		if (File.Exists(filePath))
		{
			// if MinDepth > zero, we can skip since the file is at depth zero
			if (Settings.MinDepth <= 0)
			{
				var fileResult = FilterToFileResult(new FileInfo(filePath));
				if (fileResult != null)
				{
					return new[] { fileResult };
				}
			}
		}
		return [];
	}

	// private IEnumerable<FileResult> GetAllFileResultsTasks()
	// {
	// 	var fileResults = new List<FileResult>();
	// 	var findOption = Settings.Recursive ? SearchOption.AllDirectories :
	// 		SearchOption.TopDirectoryOnly;
	// 	var findTasks = new Task<List<FileResult>>[Settings.Paths.Count];
	// 	var currentTask = 0;
	// 	foreach (var p in Settings.Paths)
	// 	{
	// 		findTasks[currentTask] = Task<List<FileResult>>.Factory.StartNew(() => GetFileResults(p).ToList());
	// 		currentTask++;
	// 	}
	// 	Task.WaitAll(findTasks);
	// 	foreach (var findTask in findTasks)
	// 	{
	// 		fileResults.AddRange(findTask.Result);
	// 	}
	// 	return fileResults;
	// }

	private List<FileResult> GetAllFileResults()
	{
		var fileResultsBag = new ConcurrentBag<FileResult>();
		Settings.Paths.AsParallel()
			.Select(GetFileResults)
			.ForAll(frs =>
			{
				foreach (var fr in frs)
				{
					fileResultsBag.Add(fr);
				}
			});

		var fileResults = fileResultsBag.ToList();
		SortFileResults(fileResults);
		return fileResults;
	}

	public IEnumerable<FileResult> Find()
	{
		var fileResults = GetAllFileResults();
		return fileResults;
	}

	private int CompareByPath(FileResult fr1, FileResult fr2)
	{
		// var cmp = Settings.SortCaseInsensitive ?
		// 	StringComparison.InvariantCultureIgnoreCase :
		// 	StringComparison.InvariantCulture;
		var cmp = Settings.SortCaseInsensitive ?
			StringComparison.OrdinalIgnoreCase :
			StringComparison.Ordinal;
		var dirNameCmp = string.Compare(fr1.File.DirectoryName, fr2.File.DirectoryName, cmp);
		return dirNameCmp == 0 ? string.Compare(fr1.File.Name, fr2.File.Name, cmp) : dirNameCmp;
	}
	
	private int CompareByName(FileResult fr1, FileResult fr2)
	{
		// var cmp = Settings.SortCaseInsensitive ?
		// 	StringComparison.InvariantCultureIgnoreCase :
		// 	StringComparison.InvariantCulture;
		var cmp = Settings.SortCaseInsensitive ?
			StringComparison.OrdinalIgnoreCase :
			StringComparison.Ordinal;
		var fileNameCmp = string.Compare(fr1.File.Name, fr2.File.Name, cmp);
		return fileNameCmp == 0 ? string.Compare(fr1.File.DirectoryName, fr2.File.DirectoryName, cmp) : fileNameCmp;
	}

	private int CompareBySize(FileResult fr1, FileResult fr2)
	{
		return fr1.File.Length == fr2.File.Length ? CompareByPath(fr1, fr2) : fr1.File.Length.CompareTo(fr2.File.Length);
	}

	private int CompareByType(FileResult fr1, FileResult fr2)
	{
		return (int) fr1.Type == (int) fr2.Type ? CompareByPath(fr1, fr2) : ((int) fr1.Type).CompareTo((int) fr2.Type);
	}

	private int CompareByLastMod(FileResult fr1, FileResult fr2)
	{
		return fr1.File.LastWriteTimeUtc == fr2.File.LastWriteTimeUtc ? CompareByPath(fr1, fr2) : fr1.File.LastWriteTimeUtc.CompareTo(fr2.File.LastWriteTimeUtc);
	}
	
	private void SortFileResults(List<FileResult> fileResults)
	{
		switch (Settings.SortBy)
		{
			case SortBy.FileName:
				fileResults.Sort(CompareByName);
				break;
			case SortBy.FileSize:
				fileResults.Sort(CompareBySize);
				break;
			case SortBy.FileType:
				fileResults.Sort(CompareByType);
				break;
			case SortBy.LastMod:
				fileResults.Sort(CompareByLastMod);
				break;
			default:
				fileResults.Sort(CompareByPath);
				break;
		}

		if (Settings.SortDescending)
		{
			fileResults.Reverse();
		}
	}

	private static List<DirectoryInfo> GetMatchingDirs(IEnumerable<FileResult> fileResults)
	{
		return
		[
			..fileResults.Where(fr => fr.File.Directory != null)
				.Select(fr => fr.File.Directory!)
				.DistinctBy(d => d.FullName)
		];
	}

	private string GetRelativePath(string path)
	{
		foreach (var p in Settings.Paths)
		{
			var relativePath = FileUtil.GetRelativePath(path, p);
			if (relativePath.Length < path.Length)
			{
				return relativePath;
			}
		}
		return path;
	}

	public void PrintMatchingDirs(IEnumerable<FileResult> fileResults)
	{
		var matchingDirs = GetMatchingDirs(fileResults)
			.Select(d => GetRelativePath(d.FullName))
			.ToList();
		if (matchingDirs.Any()) {
			Logger.Log($"\nMatching directories ({matchingDirs.Count}):");
			foreach (var d in matchingDirs)
			{
				Logger.Log(d);
			}
		} else {
			Logger.Log("\nMatching directories: 0");
		}
	}

	private static List<FileInfo> GetMatchingFiles(IEnumerable<FileResult> fileResults)
	{
		return new List<FileInfo>(
			fileResults
				.Select(fr => fr.File));
	}

	public void PrintMatchingFiles(IEnumerable<FileResult> fileResults)
	{
		var matchingFiles = GetMatchingFiles(fileResults)
			.Select(f => GetRelativePath(f.FullName))
			.ToList();
		if (matchingFiles.Any()) {
			Logger.Log($"\nMatching files ({matchingFiles.Count}):");
			foreach (var f in matchingFiles)
			{
				Logger.Log(f);
			}
		} else {
			Logger.Log("\nMatching files: 0");
		}
	}
}
