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
		_fileTypes = new FileTypes();
		_enumerationOptions = GetEnumerationOptionsForSettings();
		ValidateSettings();
	}

	private void ValidateSettings()
	{
		if (Settings.Paths == null || Settings.Paths.Count == 0)
			throw new FindException(FindError.StartpathNotDefined);

		foreach (var p in Settings.Paths)
		{
			if (!p.Exists)
			{
				throw new FindException(FindError.StartpathNotFound);
			}

			if (p.IsSymlink)
			{
				if (!Settings.FollowSymlinks)
				{
					throw new FindException(FindError.StartpathNotMatchFindSettings);
				}
			}
			else if (p.IsDirectory)
			{
				if (!IsTraversableDirPath(p))
				{
					throw new FindException(FindError.StartpathNotMatchFindSettings);
				}
			}
			else if (p.IsFile)
			{
				if (FilterToFileResult(p) == null)
				{
					throw new FindException(FindError.StartpathNotMatchFindSettings);
				}
			}
			else
			{
				// TODO: start path is unknown/invalid type
				throw new FindException(FindError.StartpathNotMatchFindSettings);
			}
		}
		if (Settings is { MaxDepth: > -1, MinDepth: > -1 } && Settings.MaxDepth < Settings.MinDepth)
		{
			throw new FindException(FindError.InvalidRangeMinDepthMaxDepth);
		}
		if (Settings is { MaxLastMod: not null, MinLastMod: not null }
		    && Settings.MaxLastMod < Settings.MinLastMod)
		{
			throw new FindException(FindError.InvalidRangeMinLastModMaxLastMod);
		}
		if (Settings is { MaxSize: > 0, MinSize: > 0 } && Settings.MaxSize < Settings.MinSize)
		{
			throw new FindException(FindError.InvalidRangeMinSizeMaxSize);
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

	private static bool EmptyOrMatchesAnyPattern(string s, ISet<Regex> patterns)
	{
		return patterns.Count == 0 || MatchesAnyPattern(s, patterns);
	}

	private static bool EmptyOrNotMatchesAnyPattern(string s, ISet<Regex> patterns)
	{
		return patterns.Count == 0 || !MatchesAnyPattern(s, patterns);
	}

	private static bool EmptyOrAnyMatchesAnyPattern(IEnumerable<string> sList, ISet<Regex> patterns)
	{
		return patterns.Count == 0 || AnyMatchesAnyPattern(sList, patterns);
	}

	private static bool EmptyOrNotAnyMatchesAnyPattern(IEnumerable<string> sList, ISet<Regex> patterns)
	{
		return patterns.Count == 0 || !AnyMatchesAnyPattern(sList, patterns);
	}

	private bool EmptyOrMatchesAnyString(string s, ISet<string> stringSet)
	{
		return stringSet.Count == 0 || stringSet.Contains(s);
	}

	private bool EmptyOrNotMatchesAnyString(string s, ISet<string> stringSet)
	{
		return stringSet.Count == 0 || !stringSet.Contains(s);
	}

	private bool EmptyOrMatchesAnyFileType(FileType fileType, ISet<FileType> fileTypeSet)
	{
		return fileTypeSet.Count == 0 || fileTypeSet.Contains(fileType);
	}

	private bool EmptyOrNotMatchesAnyFileType(FileType fileType, ISet<FileType> fileTypeSet)
	{
		return fileTypeSet.Count == 0 || !fileTypeSet.Contains(fileType);
	}

	private bool IsMatchingPathBySymlink(FilePath path)
	{
		return Settings.FollowSymlinks || !path.IsSymlink;
	}

	private bool IsMatchingDirPathByHidden(FilePath dirPath)
	{
		return Settings.IncludeHidden || !FileUtil.IsHiddenFilePath(dirPath);
	}

	private bool IsMatchingDirPathByInPatterns(FilePath dirPath)
	{
		var elems = FileUtil.GetPathElems(dirPath).ToList();
		return EmptyOrAnyMatchesAnyPattern(elems, Settings.InDirPatterns);
	}

	private bool IsMatchingDirPathByOutPatterns(FilePath dirPath)
	{
		var elems = FileUtil.GetPathElems(dirPath).ToList();
		return EmptyOrNotAnyMatchesAnyPattern(elems, Settings.OutDirPatterns);
	}

	private bool IsTraversableDirPath(FilePath dirPath)
	{
		return IsMatchingPathBySymlink(dirPath)
		       && IsMatchingDirPathByHidden(dirPath)
		       && IsMatchingDirPathByOutPatterns(dirPath);
	}

	public bool IsMatchingDirPath(FilePath dirPath)
	{
		return IsMatchingPathBySymlink(dirPath)
		       && IsMatchingDirPathByHidden(dirPath)
		       && IsMatchingDirPathByInPatterns(dirPath)
		       && IsMatchingDirPathByOutPatterns(dirPath);
	}

	private bool IsNullOrMatchingDirPath(FilePath? dirPath)
	{
		if (dirPath == null || string.IsNullOrEmpty(dirPath.ToString()))
		{
			return true;
		}

		return IsMatchingDirPath(dirPath);
	}

	private bool IsMatchingArchiveExtension(string ext)
	{
		return EmptyOrMatchesAnyString(ext, Settings.InArchiveExtensions)
		       && EmptyOrNotMatchesAnyString(ext, Settings.OutArchiveExtensions);
	}

	private bool IsMatchingArchiveExtensionForFilePath(FilePath filePath)
	{
		if (Settings.InArchiveExtensions.Count > 0 || Settings.OutArchiveExtensions.Count > 0)
		{
			return IsMatchingArchiveExtension(filePath.Extension);
		}

		return true;
	}

	private bool IsMatchingArchiveFileName(string fileName)
	{
		return EmptyOrMatchesAnyPattern(fileName, Settings.InArchiveFilePatterns)
		       && EmptyOrNotMatchesAnyPattern(fileName, Settings.OutArchiveFilePatterns);
	}

	private bool IsMatchingArchiveFileNameForFilePath(FilePath filePath)
	{
		if (Settings.InArchiveFilePatterns.Count > 0 || Settings.OutArchiveFilePatterns.Count > 0)
		{
			return IsMatchingArchiveFileName(filePath.Name);
		}

		return true;
	}

	private bool IsMatchingArchiveFilePath(FilePath filePath)
	{
		return IsMatchingArchiveExtensionForFilePath(filePath)
		       && IsMatchingArchiveFileNameForFilePath(filePath);
	}

	public bool IsMatchingArchiveFileResult(FileResult fr)
	{
		return IsNullOrMatchingDirPath(fr.FilePath.Parent)
		       && IsMatchingArchiveFilePath(fr.FilePath);
	}

	private bool IsMatchingFileNameByHidden(string fileName)
	{
		return Settings.IncludeHidden || !FileUtil.IsHiddenName(fileName);
	}

	private bool IsMatchingExtension(string ext)
	{
		return EmptyOrMatchesAnyString(ext, Settings.InExtensions)
		       && EmptyOrNotMatchesAnyString(ext, Settings.OutExtensions);
	}

	private bool IsMatchingExtensionForFilePath(FilePath filePath)
	{
		if (Settings.InExtensions.Count > 0 || Settings.OutExtensions.Count > 0)
		{
			return IsMatchingExtension(filePath.Extension);
		}

		return true;
	}

	private bool IsMatchingFileName(string fileName)
	{
		return EmptyOrMatchesAnyPattern(fileName, Settings.InFilePatterns)
		       && EmptyOrNotMatchesAnyPattern(fileName, Settings.OutFilePatterns);
	}

	private bool IsMatchingFileNameForFilePath(FilePath filePath)
	{
		if (Settings.InFilePatterns.Count > 0 || Settings.OutFilePatterns.Count > 0)
		{
			return IsMatchingFileName(filePath.Name);
		}

		return true;
	}

	private bool IsMatchingFileType(FileType fileType)
	{
		return EmptyOrMatchesAnyFileType(fileType, Settings.InFileTypes)
		       && EmptyOrNotMatchesAnyFileType(fileType, Settings.OutFileTypes);
	}

	private bool IsMatchingFileSize(long fileSize)
	{
		return (Settings.MaxSize == 0 || fileSize <= Settings.MaxSize)
		       && (Settings.MinSize == 0 || fileSize >= Settings.MinSize);
	}

	private bool IsMatchingLastMod(DateTime lastMod)
	{
		return (Settings.MaxLastMod == null || lastMod <= Settings.MaxLastMod)
		       && (Settings.MinLastMod == null || lastMod >= Settings.MinLastMod);
	}

	private bool IsMatchingFilePath(FilePath filePath)
	{
		return IsMatchingPathBySymlink(filePath)
		       && IsMatchingExtensionForFilePath(filePath)
		       && IsMatchingFileNameForFilePath(filePath);
	}

	public bool IsMatchingFileResult(FileResult fr)
	{
		return IsNullOrMatchingDirPath(fr.FilePath.Parent)
		       && IsMatchingFilePath(fr.FilePath)
		       && IsMatchingFileType(fr.Type)
		       && IsMatchingFileSize(fr.FilePath.Length)
		       && IsMatchingLastMod(fr.FilePath.LastWriteTimeUtc);
	}

	private FileResult? FilterArchiveFilePathToFileResult(FilePath filePath, FileType fileType)
	{
		if (!Settings.IncludeArchives && !Settings.ArchivesOnly)
		{
			return null;
		}

		if (!IsMatchingArchiveFilePath(filePath))
		{
			return null;
		}

		return new FileResult(filePath, fileType);
	}

	private FileResult? FilterRegularFilePathToFileResult(FilePath filePath, FileType fileType)
	{
		if (Settings.ArchivesOnly)
		{
			return null;
		}

		if (!IsMatchingFilePath(filePath) || !IsMatchingFileType(fileType)
		                                  || !IsMatchingFileSize(filePath.Length)
		                                  || !IsMatchingLastMod(filePath.LastWriteTimeUtc))
		{
			return null;
		}

		return new FileResult(filePath, fileType);
	}

	public FileResult? FilterToFileResult(FilePath filePath)
	{
		if (!IsMatchingPathBySymlink(filePath))
		{
			return null;
		}

		if (!IsNullOrMatchingDirPath(filePath.Parent) || !IsMatchingFileNameByHidden(filePath.Name))
		{
			return null;
		}

		var fileType = _fileTypes.GetFileType(filePath);
		if (fileType == FileType.Archive)
		{
			return FilterArchiveFilePathToFileResult(filePath, fileType);
		}

		return FilterRegularFilePathToFileResult(filePath, fileType);
	}

	private List<FileResult> RecGetFileResults(FilePath dirPath, int minDepth, int maxDepth, int currentDepth)
	{
		if (maxDepth > -1 && currentDepth > maxDepth)
		{
			return [];
		}

		var pathResults = new List<FileResult>();
		var recurse = maxDepth < 0 || currentDepth < maxDepth;

		var symlinkFilter = (FilePath filePath) => true;
		if (!Settings.FollowSymlinks)
		{
			symlinkFilter = filePath => !filePath.IsSymlink;
		}

		if (minDepth < 0 || currentDepth >= minDepth)
		{
			pathResults.AddRange(dirPath.EnumerateFiles("*", _enumerationOptions)
				.Where(symlinkFilter)
				.Select(FilterToFileResult)
				.Where(fr => fr != null)
				.Select(fr => fr!));
		}

		if (recurse)
		{
			var pathDirs = dirPath.EnumerateDirectories()
				.Where(symlinkFilter)
				.Where(IsTraversableDirPath);
			foreach (var pathDir in pathDirs)
			{
				pathResults.AddRange(RecGetFileResults(pathDir, minDepth, maxDepth, currentDepth + 1));
			}
		}

		return pathResults;
	}

	private List<FileResult> GetFileResults(FilePath path)
	{
		if (!Settings.FollowSymlinks && path.IsSymlink)
		{
			throw new FindException(FindError.StartpathNotMatchFindSettings);
		}
		if (path.IsDirectory)
		{
			// if MaxDepth is zero, we can skip since a directory cannot be a result
			if (Settings.MaxDepth == 0)
			{
				return [];
			}
			if (IsTraversableDirPath(path))
			{
				var maxDepth = Settings.Recursive ? Settings.MaxDepth : 1;
				return RecGetFileResults(path, Settings.MinDepth, maxDepth, 1);
			}

			throw new FindException(FindError.StartpathNotMatchFindSettings);
		}
		if (path.IsFile)
		{
			// if MinDepth > zero, we can skip since the file is at depth zero
			if (Settings.MinDepth > 0)
			{
				return [];
			}

			var fileResult = FilterToFileResult(path);
			return fileResult == null
				? throw new FindException(FindError.StartpathNotMatchFindSettings)
				: [fileResult];
		}

		throw new FindException(FindError.StartpathNotMatchFindSettings);
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
