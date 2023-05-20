using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace CsFindLib;

public static class FileUtil
{
	private const string CurrentPath = ".";
	private const string ParentPath = "..";
	private static readonly ISet<string> DotDirs = new HashSet<string> { CurrentPath, ParentPath };

	private const char ForwardSlash = '/';
	private const char BackSlash = '\\';
	private static readonly char[] DirSeps = { ForwardSlash, BackSlash };

	public static string GetHomePath()
	{
		return Environment.GetEnvironmentVariable("HOME")
		       ?? Environment.GetEnvironmentVariable("USERPROFILE")
		       ?? "~";
	}

	public static IEnumerable<string> EnumerableStringFromFile(FileResult f, Encoding enc)
	{
		return EnumerableStringFromFile(f.FullName, enc);
	}

	public static IEnumerable<string> EnumerableStringFromFile(string filePath, Encoding enc)
	{
		using var sr = new StreamReader(filePath, enc);
		// read each line, ensuring not null (EOF)
		string? line;
		while ((line = sr.ReadLine()) != null)
		{
			// return trimmed line
			yield return line;
		}
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

	public static string GetFileContents(FileResult f, Encoding encoding)
	{
		return GetFileContents(f.FullName, encoding);
	}

	public static string ExpandPath(string filePath)
	{
		return filePath[0] == '~' ? JoinPath(GetHomePath(), filePath.Substring(1)) : filePath;
	}

	public static string ContractPath(string filePath)
	{
		return filePath[0] == '~' ? filePath : filePath.Replace(GetHomePath(), "~");
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

	public static string GetRelativePath(string path, IEnumerable<string> paths)
	{
		foreach (var p in paths)
		{
			var relativePath = GetRelativePath(path, p);
			if (relativePath.Length < path.Length)
			{
				return relativePath;
			}
		}
		return path;
	}

	public static string ContractOrRelativePath(string fullPath, string startPath)
	{
		return startPath[0] == '~' ? ContractPath(fullPath) : GetRelativePath(fullPath, startPath);
	}

	public static string ContractOrRelativePath(string fullPath, IEnumerable<string> paths)
	{
		foreach (var p in paths)
		{
			var modifiedPath = ContractOrRelativePath(fullPath, p);
			if (modifiedPath.Length < fullPath.Length)
			{
				return modifiedPath;
			}
		}
		return fullPath;
	}

	public static bool IsDotDir(string fileName)
	{
		return DotDirs.Contains(NormalizePath(fileName));
	}

	public static bool IsHidden(string filePath)
	{
		return (filePath.StartsWith(CurrentPath) && !IsDotDir(filePath));
	}

	public static bool IsHiddenFile(FileSystemInfo f)
	{
		// return ((f.Name.StartsWith(CurrentPath) && !IsDotDir(f.Name))
		//         || (f.Exists && (f.Attributes & FileAttributes.Hidden) != 0));
		return (((f.Attributes & FileAttributes.Hidden) != 0)
		        || (f.Name.StartsWith(CurrentPath) && !IsDotDir(f.Name)));
	}

	public static string JoinPath(string path1, string path2)
	{
		var dirSep = ForwardSlash;
		if (path1.IndexOf(BackSlash) > -1)
			dirSep = BackSlash;
		if (path2[0] == ForwardSlash || path2[0] == BackSlash)
			path2 = path2.Substring(1);
		return NormalizePath(path1) + dirSep + path2;
	}

	public static string NormalizePath(string path)
	{
		return path.TrimEnd(DirSeps);
	}
}
