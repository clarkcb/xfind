using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace CsFindLib
{
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

		public bool IsFindDirectory(DirectoryInfo d)
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

		public bool IsFindFile(FindFile sf)
		{
			return (Settings.InExtensions.Count == 0 ||
			        Settings.InExtensions.Contains(sf.File.Extension)) &&
			       (Settings.OutExtensions.Count == 0 ||
			        !Settings.OutExtensions.Contains(sf.File.Extension)) &&
			       (Settings.InFilePatterns.Count == 0 ||
			        Settings.InFilePatterns.Any(p => p.Match(sf.File.Name).Success)) &&
			       (Settings.OutFilePatterns.Count == 0 ||
			        !Settings.OutFilePatterns.Any(p => p.Match(sf.File.Name).Success)) &&
			       (Settings.InFileTypes.Count == 0 ||
			        Settings.InFileTypes.Contains(sf.Type)) &&
			       (Settings.OutFileTypes.Count == 0 ||
			        !Settings.OutFileTypes.Contains(sf.Type));
		}

		public bool IsArchiveFindFile(FindFile sf)
		{
			return (Settings.InArchiveExtensions.Count == 0 ||
			        Settings.InArchiveExtensions.Contains(sf.File.Extension)) &&
			       (Settings.OutArchiveExtensions.Count == 0 ||
			        !Settings.OutArchiveExtensions.Contains(sf.File.Extension)) &&
			       (Settings.InArchiveFilePatterns.Count == 0 ||
			        Settings.InArchiveFilePatterns.Any(p => p.Match(sf.File.Name).Success)) &&
			       (Settings.OutArchiveFilePatterns.Count == 0 ||
			        !Settings.OutArchiveFilePatterns.Any(p => p.Match(sf.File.Name).Success));
		}

		public bool FilterFile(FindFile sf)
		{
			if (Settings.ExcludeHidden && FileUtil.IsHiddenFile(sf.File))
				return false;
			if (sf.Type.Equals(FileType.Archive))
			{
				return (Settings.IncludeArchives && IsArchiveFindFile(sf));
			}
			return (!Settings.ArchivesOnly && IsFindFile(sf));
		}

		private IEnumerable<FindFile> GetFindFiles()
		{
			var findFiles = new List<FindFile>();
			var findOption = Settings.Recursive ? SearchOption.AllDirectories :
				SearchOption.TopDirectoryOnly;
			foreach (var p in Settings.Paths)
			{
				var expandedPath = FileUtil.ExpandPath(p);
				if (Directory.Exists(expandedPath))
				{
					findFiles.AddRange(new DirectoryInfo(expandedPath).
						EnumerateFiles("*", findOption).
						Where(f => f.Directory == null || IsFindDirectory(f.Directory)).
						Select(f => new FindFile(f, _fileTypes.GetFileType(f))).
						Where(FilterFile).
						Select(f => f));
				}
				else if (File.Exists(expandedPath))
				{
					var fi = new FileInfo(expandedPath);
					var ff = new FindFile(fi, _fileTypes.GetFileType(fi));
					if (FilterFile(ff))
					{
						findFiles.Add(ff);
					}
				}
			}
			return findFiles;
		}

		public IEnumerable<FindFile> Find()
		{
			var findFiles = GetFindFiles().ToList();
			return findFiles;
		}

		private IEnumerable<DirectoryInfo> GetMatchingDirs(IEnumerable<FindFile> findFiles)
		{
			return new List<DirectoryInfo>(
				findFiles.Where(ff => ff.File.Directory != null)
					.Select(ff => ff.File.Directory!)
					.Distinct()
					.OrderBy(d => d.FullName));
		}

		private string GetRelativePath(string path)
		{
			foreach (var p in Settings.Paths)
			{
				string relativePath = FileUtil.GetRelativePath(path, p);
				if (relativePath.Length < path.Length)
				{
					return relativePath;
				}
			}
			return path;
		}

		public void PrintMatchingDirs(IEnumerable<FindFile> findFiles)
		{
			var matchingDirs = GetMatchingDirs(findFiles)
				.Select(d => GetRelativePath(d.FullName))
				.Distinct()
				.OrderBy(d => d).ToList();
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

		private IEnumerable<FileInfo> GetMatchingFiles(IEnumerable<FindFile> findFiles)
		{
			return new List<FileInfo>(
				findFiles
					.Select(f => f.File.ToString())
					.Distinct().Select(f => new FileInfo(f))
					.OrderBy(d => d.FullName));
		}

		public void PrintMatchingFiles(IEnumerable<FindFile> findFiles)
		{
			var matchingFiles = GetMatchingFiles(findFiles)
				.Select(f => GetRelativePath(f.FullName))
				.Distinct()
				.OrderBy(f => f).ToList();
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

	internal class CaseInsensitiveComparer : IComparer<string>
	{
		public int Compare(string? a, string? b)
		{
			if (a == null && b == null)
				return 0;
			if (a == null)
				return -1;
			if (b == null)
				return 1;
			return string.Compare(a.ToUpper(), b.ToUpper(), StringComparison.Ordinal);
		}
	}
}
