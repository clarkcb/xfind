using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace CsFindLib;

public class Finder
{
	private readonly FileTypes _fileTypes;
	private FindSettings Settings { get; }
	public Finder(FindSettings settings)
	{
		Settings = settings;
		ValidateSettings();
		_fileTypes = new FileTypes();
	}

	private void ValidateSettings()
	{
		if (Settings.Paths.Count == 0)
			throw new FindException("Startpath not defined");
		if (Settings.Paths.Select(FileUtil.ExpandPath).Any(p => !Directory.Exists(p) && !File.Exists(p)))
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

	public bool IsMatchingDirectory(DirectoryInfo d)
	{
		if (!Settings.IncludeHidden)
		{
			if (d.FullName.Split('/', '\\').ToList()
			    .Where(e => !string.IsNullOrEmpty(e))
			    .Any(e => FileUtil.IsHidden(e)))
			{
				return false;
			}
		}

		return (Settings.InDirPatterns.Count == 0 ||
		        Settings.InDirPatterns.Any(p => p.Matches(d.FullName).Count > 0)) &&
		       (Settings.OutDirPatterns.Count == 0 ||
		        !Settings.OutDirPatterns.Any(p => p.Matches(d.FullName).Count > 0));
	}

	public bool IsMatchingFileResult(FileResult fr)
	{
		if ((Settings.InExtensions.Count > 0 &&
			!Settings.InExtensions.Contains(fr.File.Extension)) ||
			(Settings.OutExtensions.Count > 0 &&
			Settings.OutExtensions.Contains(fr.File.Extension)))
		{
			return false;
		}
		if ((Settings.InFilePatterns.Count > 0 &&
			!Settings.InFilePatterns.Any(p => p.Match(fr.File.Name).Success)) ||
			(Settings.OutFilePatterns.Count > 0 &&
			Settings.OutFilePatterns.Any(p => p.Match(fr.File.Name).Success)))
		{
			return false;
		}
		if ((Settings.InFileTypes.Count > 0 &&
			!Settings.InFileTypes.Contains(fr.Type)) ||
			(Settings.OutFileTypes.Count > 0 &&
			Settings.OutFileTypes.Contains(fr.Type)))
		{
			return false;
		}
		if ((Settings.MaxLastMod != null && fr.File.LastWriteTimeUtc > Settings.MaxLastMod) ||
			(Settings.MinLastMod != null && fr.File.LastWriteTimeUtc < Settings.MinLastMod))
		{
			return false;
		}
		if ((Settings.MaxSize > 0 && fr.File.Length > Settings.MaxSize) ||
			(Settings.MinSize > 0 && fr.File.Length < Settings.MinSize))
		{
			return false;
		}
		return true;
	}

	public bool IsMatchingArchiveFile(FileResult fr)
	{
		return (Settings.InArchiveExtensions.Count == 0 ||
		        Settings.InArchiveExtensions.Contains(fr.File.Extension)) &&
		       (Settings.OutArchiveExtensions.Count == 0 ||
		        !Settings.OutArchiveExtensions.Contains(fr.File.Extension)) &&
		       (Settings.InArchiveFilePatterns.Count == 0 ||
		        Settings.InArchiveFilePatterns.Any(p => p.Match(fr.File.Name).Success)) &&
		       (Settings.OutArchiveFilePatterns.Count == 0 ||
		        !Settings.OutArchiveFilePatterns.Any(p => p.Match(fr.File.Name).Success));
	}

	public FileResult? FilterToFileResult(FileInfo fi)
	{
		if (!Settings.IncludeHidden && FileUtil.IsHiddenFile(fi))
			return null;
		var fr = new FileResult(fi, _fileTypes.GetFileType(fi));
		if (fr.Type.Equals(FileType.Archive))
		{
			if (Settings.IncludeArchives && IsMatchingArchiveFile(fr))
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

	private IEnumerable<FileResult> GetFileResults(string filePath)
	{
		var findOption = Settings.Recursive ? SearchOption.AllDirectories :
			SearchOption.TopDirectoryOnly;
		var expandedPath = FileUtil.ExpandPath(filePath);
		var pathSepCount = FileUtil.SepCount(expandedPath);
		var pathResults = new List<FileResult>();

		bool MatchDirectory(FileInfo f, int startPathSepCount)
		{
			if (f.Directory == null) return true;
			var fileSepCount = FileUtil.SepCount(f.FullName);
			var depth = fileSepCount - startPathSepCount;
			return depth >= Settings.MinDepth
			       && (Settings.MaxDepth < 1 || depth <= Settings.MaxDepth)
			       && IsMatchingDirectory(f.Directory);
		}

		if (Directory.Exists(expandedPath))
		{
			// if MaxDepth is zero, we can skip since a directory cannot be a result
			if (Settings.MaxDepth != 0)
			{
				// 1) Sequential file processing
				pathResults.AddRange(new DirectoryInfo(expandedPath).EnumerateFiles("*", findOption)
					.Where(f => MatchDirectory(f, pathSepCount))
					.Select(FilterToFileResult)
					.Where(fr => fr != null)
					.Select(fr => fr!));

				// // 2) Parallel file processing - this is actually slower than 1
				// var fileResultBag = new ConcurrentBag<FileResult>();
				// new DirectoryInfo(expandedPath).EnumerateFiles("*", findOption)
				// 	.AsParallel()
				// 	.Where(f => MatchDirectory(f, pathSepCount))
				// 	.Select(FilterToFileResult)
				// 	.Where(fr => fr != null)
				// 	.ForAll(fr => 
				// 	{
				// 		fileResultBag.Add(fr!);
				// 	});
				// pathResults.AddRange(fileResultBag);
			}
		}
		else if (File.Exists(expandedPath))
		{
			// if MinDepth > zero, we can skip since the file is at depth zero
			if (Settings.MinDepth <= 0)
			{
				var fi = new FileInfo(expandedPath);
				var fr = FilterToFileResult(fi);
				if (fr != null)
				{
					pathResults.Add(fr);
				}
			}
		}

		return pathResults;
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

		return fileResultsBag.ToList();
	}

	public IEnumerable<FileResult> Find()
	{
		var fileResults = GetAllFileResults().ToList();
		SortFileResults(fileResults);
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
