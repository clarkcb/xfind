using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace CsFindLib;

public static class FileUtil
{
	private const string CurrentPath = ".";
	private const string ParentPath = "..";
	private static readonly HashSet<string> DotDirs = [CurrentPath, ParentPath];

	public static string GetHomePath()
	{
		return Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
	}

	public static string NormalizePath(string path)
	{
		return path.TrimEnd(Path.DirectorySeparatorChar);
	}

	public static string ExpandPath(string filePath)
	{
		ArgumentNullException.ThrowIfNull(filePath);
		if (filePath.Length == 0) throw new ArgumentException("Invalid path", nameof(filePath));
		if (filePath[0] != '~') return filePath;
		var sepIndex = filePath.IndexOf(Path.DirectorySeparatorChar);
		var userPath = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
		if (filePath != "~" && sepIndex != 1)
		{
			// Another user's home directory
			var homePath = Path.GetDirectoryName(userPath) ?? ".";
			var userName = sepIndex == -1 ? filePath[1..] : filePath.Substring(1, sepIndex - 1);
			userPath = Path.Join(homePath, userName);
		}
		return Path.Join(userPath, filePath[sepIndex..]);
	}

	public static string GetRelativePath(string fullPath, string startPath)
	{
		var filePath = NormalizePath(fullPath);
		startPath = NormalizePath(startPath);
		var startFullPath = NormalizePath(new DirectoryInfo(startPath).FullName);
		if (startFullPath != startPath)
		{
			filePath = filePath.Replace(startFullPath, startPath);
		}
		return filePath;
	}

	public static List<string> GetPathElems(FilePath filePath)
	{
		return filePath.Path.Split(Path.DirectorySeparatorChar)
			.Where(e => !string.IsNullOrWhiteSpace(e)).ToList();
	}

	public static bool IsDotDir(string fileName)
	{
		return DotDirs.Contains(NormalizePath(fileName));
	}

	public static bool IsHidden(string fileName)
	{
		return fileName.StartsWith(CurrentPath) && !IsDotDir(fileName);
	}

	public static bool IsHiddenFile(FileSystemInfo f)
	{
		// TODO: the attributes check seems to return some false positives
		return (f.Name.StartsWith(CurrentPath) && !IsDotDir(f.Name))
		       || (f.Exists && (f.Attributes & FileAttributes.Hidden) != 0);
	}

	public static bool IsHiddenFilePath(FilePath filePath)
	{
		return IsHiddenFile(filePath.File);
	}

	public static string GetFileContents(string filePath, Encoding encoding)
	{
		try
		{
			using var sr = new StreamReader(filePath, encoding);
			var contents = sr.ReadToEnd();
			return contents;
		}
		catch (IOException e)
		{
			throw new FindException(e.Message);
		}
	}
}
