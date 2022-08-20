﻿using System;
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

	public static IEnumerable<string> EnumerableStringFromFile(string filepath, Encoding enc)
	{
		using var sr = new StreamReader(filepath, enc);
		// read each line, ensuring not null (EOF)
		string? line;
		while ((line = sr.ReadLine()) != null)
		{
			// return trimmed line
			yield return line;
		}
	}

	public static string GetFileContents(string filepath, Encoding encoding)
	{
		try
		{
			using var sr = new StreamReader(filepath, encoding);
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

	public static string ExpandPath(string filepath)
	{
		return filepath[0] == '~' ? JoinPath(GetHomePath(), filepath.Substring(1)) : filepath;
	}

	public static string ContractPath(string filepath)
	{
		return filepath[0] == '~' ? filepath : filepath.Replace(GetHomePath(), "~");
	}

	public static string GetRelativePath(string fullPath, string startpath)
	{
		var filePath = NormalizePath(fullPath);
		startpath = NormalizePath(startpath);
		var startFullPath = NormalizePath(new DirectoryInfo(startpath).FullName);
		if (startFullPath != startpath)
		{
			filePath = filePath.Replace(startFullPath, startpath);
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

	public static string ContractOrRelativePath(string fullPath, string startpath)
	{
		return startpath[0] == '~' ? ContractPath(fullPath) : GetRelativePath(fullPath, startpath);
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

	public static bool IsDotDir(string filename)
	{
		return DotDirs.Contains(NormalizePath(filename));
	}

	public static bool IsHidden(string filepath)
	{
		return (filepath.StartsWith(CurrentPath) && !IsDotDir(filepath));
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
