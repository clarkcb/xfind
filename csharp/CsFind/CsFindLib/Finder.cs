using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace CsFindLib;

public class Finder
{
	private const string StartpathNotDefined = "Startpath not defined";
	private const string StartpathNotFound = "Startpath not found";
	private const string InvalidRangeMinDepthMaxDepth = "Invalid range for mindepth and maxdepth";
	private const string InvalidRangeMinLastModMaxLastMod = "Invalid range for minlastmod and maxlastmod";
	private const string InvalidRangeMinSizeMaxSize = "Invalid range for minsize and maxsize";
	private const string StartpathNotMatchFindSettings = "Startpath does not match find settings";

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
			throw new FindException(StartpathNotDefined);
		if (Settings.Paths.Any(p => !p.Exists))
		{
			throw new FindException(StartpathNotFound);
		}
		if (Settings is { MaxDepth: > -1, MinDepth: > -1 } && Settings.MaxDepth < Settings.MinDepth)
		{
			throw new FindException(InvalidRangeMinDepthMaxDepth);
		}
		if (Settings is { MaxLastMod: not null, MinLastMod: not null }
		    && Settings.MaxLastMod < Settings.MinLastMod)
		{
			throw new FindException(InvalidRangeMinLastModMaxLastMod);
		}
		if (Settings is { MaxSize: > 0, MinSize: > 0 } && Settings.MaxSize < Settings.MinSize)
		{
			throw new FindException(InvalidRangeMinSizeMaxSize);
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

	public bool FilterDirectoryByHidden(FilePath d)
	{
		if (string.IsNullOrEmpty(d.Path))
		{
			return true;
		}

		if (!Settings.IncludeHidden)
		{
			return !FileUtil.IsHiddenFilePath(d);
		}

		return true;
	}

	public bool FilterDirectoryByInPatterns(FilePath d)
	{
		if (string.IsNullOrEmpty(d.Path))
		{
			return true;
		}

		var elems = FileUtil.GetPathElems(d).ToList();

		return Settings.InDirPatterns.Count == 0
		       || AnyMatchesAnyPattern(elems, Settings.InDirPatterns);
	}

	public bool FilterDirectoryByOutPatterns(FilePath d)
	{
		if (string.IsNullOrEmpty(d.Path))
		{
			return false;
		}

		var elems = FileUtil.GetPathElems(d).ToList();

		return Settings.OutDirPatterns.Count == 0
		       || !AnyMatchesAnyPattern(elems, Settings.OutDirPatterns);
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
		
		return FilterDirectoryByHidden(d) && FilterDirectoryByInPatterns(d) && FilterDirectoryByOutPatterns(d);
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
		if (filePath.Parent != null && !IsMatchingDirectory(filePath.Parent))
			return null;
		if (!Settings.IncludeHidden && FileUtil.IsHiddenName(filePath.Name))
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
			var pathDirs = dirPath.EnumerateDirectories()
				.Where(d => FilterDirectoryByHidden(d) && FilterDirectoryByOutPatterns(d));
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
			if (FilterDirectoryByHidden(filePath) && FilterDirectoryByOutPatterns(filePath))
			{
				var maxDepth = Settings.Recursive ? Settings.MaxDepth : 1;
				return RecGetFileResults(filePath, Settings.MinDepth, maxDepth, 1);
			}

			throw new FindException(StartpathNotMatchFindSettings);
		}

		// if MinDepth > zero, we can skip since the file is at depth zero
		if (Settings.MinDepth > 0)
		{
			return [];
		}

		var fileResult = FilterToFileResult(filePath);
		return fileResult == null
			? throw new FindException(StartpathNotMatchFindSettings)
			: [fileResult];
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
		var fileResultSorter = new FileResultSorter(Settings);
		fileResultSorter.Sort(fileResults);
		return fileResults;
	}

	public List<FileResult> Find()
	{
		var fileResults = GetAllFileResults();
		return fileResults;
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

	public static void PrintMatchingDirs(IEnumerable<FileResult> fileResults, FileResultFormatter formatter)
	{
		var matchingDirs = GetMatchingDirs(fileResults);
		if (matchingDirs.Count != 0)
		{
			Logger.Log($"\nMatching directories ({matchingDirs.Count}):");
			foreach (var d in matchingDirs)
			{
				Logger.Log(formatter.FormatDirPath(d));
			}
		}
		else
		{
			Logger.Log("\nMatching directories: 0");
		}
	}

	public static void PrintMatchingFiles(IEnumerable<FileResult> fileResults, FileResultFormatter formatter)
	{
		var fileResultsList = fileResults.ToList();
		if (fileResultsList.Count != 0)
		{
			Logger.Log($"\nMatching files ({fileResultsList.Count}):");
			foreach (var fileResult in fileResultsList)
			{
				Logger.Log(formatter.FormatFileResult(fileResult));
			}
		}
		else
		{
			Logger.Log("\nMatching files: 0");
		}
	}
}
