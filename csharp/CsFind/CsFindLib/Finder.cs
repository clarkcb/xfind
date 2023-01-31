using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace CsFindLib;

public class Finder
{
	private readonly FileTypes _fileTypes;
	private FindSettings Settings { get; set; }
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
	}

	public bool IsMatchingDirectory(DirectoryInfo d)
	{
		if (Settings.ExcludeHidden)
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

	public bool IsMatchingFile(FileResult fr)
	{
		return (Settings.InExtensions.Count == 0 ||
		        Settings.InExtensions.Contains(fr.File.Extension)) &&
		       (Settings.OutExtensions.Count == 0 ||
		        !Settings.OutExtensions.Contains(fr.File.Extension)) &&
		       (Settings.InFilePatterns.Count == 0 ||
		        Settings.InFilePatterns.Any(p => p.Match(fr.File.Name).Success)) &&
		       (Settings.OutFilePatterns.Count == 0 ||
		        !Settings.OutFilePatterns.Any(p => p.Match(fr.File.Name).Success)) &&
		       (Settings.InFileTypes.Count == 0 ||
		        Settings.InFileTypes.Contains(fr.Type)) &&
		       (Settings.OutFileTypes.Count == 0 ||
		        !Settings.OutFileTypes.Contains(fr.Type));
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
		if (Settings.ExcludeHidden && FileUtil.IsHiddenFile(fi))
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
		if (!Settings.ArchivesOnly && IsMatchingFile(fr))
		{
			return fr;
		}

		return null;
	}

	private IEnumerable<FileResult> GetFileResults()
	{
		var fileResults = new List<FileResult>();
		var findOption = Settings.Recursive ? SearchOption.AllDirectories :
			SearchOption.TopDirectoryOnly;
		foreach (var p in Settings.Paths)
		{
			var expandedPath = FileUtil.ExpandPath(p);
			if (Directory.Exists(expandedPath))
			{
				fileResults.AddRange(new DirectoryInfo(expandedPath).
					EnumerateFiles("*", findOption).
					Where(f => f.Directory == null || IsMatchingDirectory(f.Directory)).
					Select(f => FilterToFileResult(f)).
					Where(fr => fr != null).
					Select(f => f!));
			}
			else if (File.Exists(expandedPath))
			{
				var fi = new FileInfo(expandedPath);
				var fr = FilterToFileResult(fi);
				if (fr != null)
				{
					fileResults.Add(fr);
				}
			}
		}
		return fileResults;
	}

	public IEnumerable<FileResult> Find()
	{
		var fileResults = GetFileResults().ToList();
		SortFileResults(fileResults);
		return fileResults;
	}

	private int CompareByPath(FileResult fr1, FileResult fr2)
	{
		var cmp = Settings.SortCaseInsensitive ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
		var dirNameCmp = string.Compare(fr1.File.DirectoryName, fr2.File.DirectoryName, cmp);
		return dirNameCmp == 0 ? string.Compare(fr1.File.Name, fr2.File.Name, cmp) : dirNameCmp;
	}
	
	private int CompareByName(FileResult fr1, FileResult fr2)
	{
		var cmp = Settings.SortCaseInsensitive ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
		var fileNameCmp = string.Compare(fr1.File.Name, fr2.File.Name, cmp);
		return fileNameCmp == 0 ? string.Compare(fr1.File.DirectoryName, fr2.File.DirectoryName, cmp) : fileNameCmp;
	}

	private static int CompareByType(FileResult fr1, FileResult fr2)
	{
		if ((int) fr1.Type == (int) fr2.Type)
		{
			return CompareByPath(fr1, fr2);
		}

		return ((int) fr1.Type).CompareTo((int) fr2.Type);
	}
	
	private void SortFileResults(List<FileResult> fileResults)
	{
		switch (Settings.SortBy)
		{
			case SortBy.FileName:
				fileResults.Sort(CompareByName);
				break;
			case SortBy.FileType:
				fileResults.Sort(CompareByType);
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

	private static IEnumerable<DirectoryInfo> GetMatchingDirs(IEnumerable<FileResult> fileResults)
	{
		return new List<DirectoryInfo>(
			fileResults.Where(fr => fr.File.Directory != null)
				.Select(fr => fr.File.Directory!)
				.DistinctBy(d => d.FullName));
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
			Common.Log($"\nMatching directories ({matchingDirs.Count}):");
			foreach (var d in matchingDirs)
			{
				Common.Log(d);
			}
		} else {
			Common.Log("\nMatching directories: 0");
		}
	}

	private static IEnumerable<FileInfo> GetMatchingFiles(IEnumerable<FileResult> fileResults)
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
			Common.Log($"\nMatching files ({matchingFiles.Count}):");
			foreach (var f in matchingFiles)
			{
				Common.Log(f);
			}
		} else {
			Common.Log("\nMatching files: 0");
		}
	}
}
