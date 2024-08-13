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
	private static readonly ISet<string> DotDirs = new HashSet<string> { CurrentPath, ParentPath };

	public static string GetHomePath()
	{
		return Environment.GetEnvironmentVariable("HOME")
		       ?? Environment.GetEnvironmentVariable("USERPROFILE")
		       ?? "~";
	}

	public static string GetFileExtension(FileInfo fi)
	{
		var ext = fi.Extension;
		if (string.IsNullOrEmpty(ext))
		{
			return "";
		}
		return ext.Substring(1);
	}

	public static string NormalizePath(string path)
	{
		return path.TrimEnd(Path.DirectorySeparatorChar);
	}

	public static string ExpandPath(string filePath)
	{
		return filePath[0] == '~' ? Path.Join(GetHomePath(), filePath[1..]) : filePath;
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

	public static IEnumerable<string> GetDirElems(DirectoryInfo dir)
	{
		var elems = new List<string> { dir.Name };
		var parent = dir.Parent;
		var root = Path.DirectorySeparatorChar.ToString();
		while (parent != null && parent.Name != root)
		{
			elems.Add(parent.Name);
			parent = parent.Parent;
		}

		return elems;
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
		// TODO: the attributes check seems to return some false positives
		return ((f.Name.StartsWith(CurrentPath) && !IsDotDir(f.Name))
		        || (f.Exists && (f.Attributes & FileAttributes.Hidden) != 0));
	}

	public static int SepCount(string path)
	{
		return path.Count(c => c == Path.DirectorySeparatorChar);
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
