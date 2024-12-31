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
		if (Settings.Paths.Any(p => !p.Exists))
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

	private static bool MatchesAnyPattern(string s, ISet<Regex> patterns)
	{
		return patterns.Any(p => p.Matches(s).Count > 0);
	}

	private static bool AnyMatchesAnyPattern(IEnumerable<string> slist, ISet<Regex> patterns)
	{
		return slist.Any(s => MatchesAnyPattern(s, patterns));
	}
	
	public bool IsMatchingDirectory(FilePath d)
	{
		if (string.IsNullOrEmpty(d.Path))
		{
			return true;
		}

		// Detect and filter out symlinked directories if !Settings.FollowSymlinks
		if (!Settings.FollowSymlinks && d.IsSymlink)
		{
			return false;
		}

		var elems = FileUtil.GetPathElems(d).ToList();

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

	private bool IsMatchingArchiveFileExtension(string ext)
	{
		return (Settings.InArchiveExtensions.Count == 0 ||
		         Settings.InArchiveExtensions.Contains(ext)) &&
		        (Settings.OutArchiveExtensions.Count == 0 ||
		         !Settings.OutArchiveExtensions.Contains(ext));
	}

	private bool HasMatchingArchiveFileExtension(FilePath filePath)
	{
		if (Settings.InArchiveExtensions.Count > 0 || Settings.OutArchiveExtensions.Count > 0)
		{
			return IsMatchingArchiveFileExtension(filePath.Extension);
		}

		return true;
	}

	private bool IsMatchingFileExtension(string ext)
	{
		return (Settings.InExtensions.Count == 0 ||
		         Settings.InExtensions.Contains(ext)) &&
		        (Settings.OutExtensions.Count == 0 ||
		         !Settings.OutExtensions.Contains(ext));
	}

	private bool HasMatchingFileExtension(FilePath filePath)
	{
		if (Settings.InExtensions.Count > 0 || Settings.OutExtensions.Count > 0)
		{
			return IsMatchingFileExtension(filePath.Extension);
		}

		return true;
	}

	private bool IsMatchingArchiveFileName(string fileName)
	{
		return (Settings.InArchiveFilePatterns.Count == 0 ||
		        Settings.InArchiveFilePatterns.Any(p => p.Match(fileName).Success)) &&
		       (Settings.OutArchiveFilePatterns.Count == 0 ||
		        !Settings.OutArchiveFilePatterns.Any(p => p.Match(fileName).Success));
	}

	private bool IsMatchingFileName(string fileName)
	{
		return (Settings.InFilePatterns.Count == 0 ||
		        Settings.InFilePatterns.Any(p => p.Match(fileName).Success)) &&
		       (Settings.OutFilePatterns.Count == 0 ||
		        !Settings.OutFilePatterns.Any(p => p.Match(fileName).Success));
	}

	private bool IsMatchingFileType(FileType fileType)
	{
		return (Settings.InFileTypes.Count == 0 ||
		        Settings.InFileTypes.Contains(fileType)) &&
		       (Settings.OutFileTypes.Count == 0 ||
		        !Settings.OutFileTypes.Contains(fileType));
	}

	private bool IsMatchingFileSize(long fileSize)
	{
		return (Settings.MaxSize == 0 || fileSize <= Settings.MaxSize) &&
		       (Settings.MinSize == 0 || fileSize >= Settings.MinSize);
	}

	private bool IsMatchingLastMod(DateTime lastMod)
	{
		return (Settings.MaxLastMod == null || lastMod <= Settings.MaxLastMod) &&
		       (Settings.MinLastMod == null || lastMod >= Settings.MinLastMod);
	}

	public bool IsMatchingFileResult(FileResult fr)
	{
		return HasMatchingFileExtension(fr.FilePath) &&
		       IsMatchingFileName(fr.FilePath.Name) &&
		       IsMatchingFileType(fr.Type) &&
		       IsMatchingFileSize(fr.FilePath.Length) &&
		       IsMatchingLastMod(fr.FilePath.LastWriteTimeUtc);
	}

	public bool IsMatchingArchiveFileResult(FileResult fr)
	{
		return HasMatchingArchiveFileExtension(fr.FilePath) &&
		       IsMatchingArchiveFileName(fr.FilePath.Name);
	}

	public FileResult? FilterToFileResult(FilePath filePath)
	{
		// Detect and filter out symlinked files unless Settings.FollowSymlinks
		if (!Settings.FollowSymlinks && filePath.IsSymlink)
			return null;
		if (!Settings.IncludeHidden && FileUtil.IsHiddenFilePath(filePath))
			return null;
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
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

	private List<FileResult> RecGetFileResults(FilePath dirPath, int minDepth, int maxDepth, int currentDepth)
	{
		if (maxDepth > -1 && currentDepth > maxDepth)
		{
			return [];
		}

		var pathResults = new List<FileResult>();
		var recurse = currentDepth != maxDepth;

		if (minDepth < 0 || currentDepth >= minDepth)
		{
			pathResults.AddRange(dirPath.EnumerateFiles("*", _enumerationOptions)
				.Select(FilterToFileResult)
				.Where(fr => fr != null)
				.Select(fr => fr!));
		}

		if (recurse)
		{
			var pathDirs = dirPath.EnumerateDirectories().Where(IsMatchingDirectory);
			foreach (var pathDir in pathDirs)
			{
				pathResults.AddRange(RecGetFileResults(pathDir, minDepth, maxDepth, currentDepth + 1));
			}
		}

		return pathResults;
	}

	private List<FileResult> GetFileResults(FilePath filePath)
	{
		if (filePath.IsDirectory)
		{
			// if MaxDepth is zero, we can skip since a directory cannot be a result
			if (Settings.MaxDepth == 0)
			{
				return [];
			}
			if (IsMatchingDirectory(filePath))
			{
				var maxDepth = Settings.Recursive ? Settings.MaxDepth : 1;
				return RecGetFileResults(filePath, Settings.MinDepth, maxDepth, 1);
			}

			throw new FindException("Startpath does not match find settings");
		}
		if (filePath.IsFile)
		{
			// if MinDepth > zero, we can skip since the file is at depth zero
			if (Settings.MinDepth <= 0)
			{
				var fileResult = FilterToFileResult(filePath);
				if (fileResult != null)
				{
					return [fileResult];
				}
				throw new FindException("Startpath does not match find settings");
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
		try
		{
			Settings.Paths.AsParallel()
				.Select(GetFileResults)
				.ForAll(frs =>
				{
					foreach (var fr in frs)
					{
						fileResultsBag.Add(fr);
					}
				});
		}
		catch (AggregateException ae)
		{
			if (ae.InnerException != null)
			{
				throw ae.InnerException;
			}
			throw new FindException("Unknown error");
		}

		var fileResults = fileResultsBag.ToList();
		SortFileResults(fileResults);
		return fileResults;
	}

	public List<FileResult> Find()
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
		var dirNameCmp = string.Compare(fr1.FilePath.Parent?.ToString(), fr2.FilePath.Parent?.ToString(), cmp);
		return dirNameCmp == 0 ? string.Compare(fr1.FilePath.Name, fr2.FilePath.Name, cmp) : dirNameCmp;
	}
	
	private int CompareByName(FileResult fr1, FileResult fr2)
	{
		// var cmp = Settings.SortCaseInsensitive ?
		// 	StringComparison.InvariantCultureIgnoreCase :
		// 	StringComparison.InvariantCulture;
		var cmp = Settings.SortCaseInsensitive ?
			StringComparison.OrdinalIgnoreCase :
			StringComparison.Ordinal;
		var fileNameCmp = string.Compare(fr1.FilePath.Name, fr2.FilePath.Name, cmp);
		return fileNameCmp == 0 ? string.Compare(fr1.FilePath.Parent?.ToString(), fr2.FilePath.Parent?.ToString(), cmp) : fileNameCmp;
	}

	private int CompareBySize(FileResult fr1, FileResult fr2)
	{
		return fr1.FilePath.Length == fr2.FilePath.Length ? CompareByPath(fr1, fr2) : fr1.FilePath.Length.CompareTo(fr2.FilePath.Length);
	}

	private int CompareByType(FileResult fr1, FileResult fr2)
	{
		return (int) fr1.Type == (int) fr2.Type ? CompareByPath(fr1, fr2) : ((int) fr1.Type).CompareTo((int) fr2.Type);
	}

	private int CompareByLastMod(FileResult fr1, FileResult fr2)
	{
		return fr1.FilePath.LastWriteTimeUtc == fr2.FilePath.LastWriteTimeUtc ? CompareByPath(fr1, fr2) : fr1.FilePath.LastWriteTimeUtc.CompareTo(fr2.FilePath.LastWriteTimeUtc);
	}

	public Comparison<FileResult> GetFileResultsComparison()
	{
		switch (Settings.SortBy)
		{
			case SortBy.FileName:
				return CompareByName;
			case SortBy.FileSize:
				return CompareBySize;
			case SortBy.FileType:
				return CompareByType;
			case SortBy.LastMod:
				return CompareByLastMod;
			default:
				return CompareByPath;
		}
	}

	private void SortFileResults(List<FileResult> fileResults)
	{
		var comparison = GetFileResultsComparison();
		fileResults.Sort(comparison);

		if (Settings.SortDescending)
		{
			fileResults.Reverse();
		}
	}

	private static List<FilePath> GetMatchingDirs(IEnumerable<FileResult> fileResults)
	{
		return
		[
			..fileResults.Where(fr => fr.FilePath.Parent != null)
				.Select(fr => fr.FilePath.Parent!)
				.DistinctBy(d => d.ToString())
		];
	}

	public static void PrintMatchingDirs(IEnumerable<FileResult> fileResults)
	{
		var matchingDirs = GetMatchingDirs(fileResults)
			.Select(d => d.ToString())
			.ToList();
		if (matchingDirs.Count != 0)
		{
			Logger.Log($"\nMatching directories ({matchingDirs.Count}):");
			foreach (var d in matchingDirs)
			{
				Logger.Log(d);
			}
		}
		else
		{
			Logger.Log("\nMatching directories: 0");
		}
	}

	private static List<FilePath> GetMatchingFiles(IEnumerable<FileResult> fileResults)
	{
		return
		[
			..fileResults
				.Select(fr => fr.FilePath)
		];
	}

	public static void PrintMatchingFiles(IEnumerable<FileResult> fileResults)
	{
		var matchingFiles = GetMatchingFiles(fileResults)
			.Select(f => f.ToString())
			.ToList();
		if (matchingFiles.Count != 0)
		{
			Logger.Log($"\nMatching files ({matchingFiles.Count}):");
			foreach (var f in matchingFiles)
			{
				Logger.Log(f);
			}
		}
		else
		{
			Logger.Log("\nMatching files: 0");
		}
	}
}
