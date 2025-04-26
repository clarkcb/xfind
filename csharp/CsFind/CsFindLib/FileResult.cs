using System.Collections.Generic;
using System.Text;
using System;

namespace CsFindLib;

public class FileResult(IList<string> containers, FilePath filePath, FileType type)
{
	public const string ContainerSeparator = "!";

	public IList<string> Containers { get; } = containers;
	public FilePath FilePath { get; } = filePath;
	public FileType Type { get; } = type;

	public string FullName => ToString();

	public string PathAndName => FilePath.ToString();

	public FileResult(FilePath filePath, FileType type) :
		this(new List<string>(), filePath, type) {}

	public FileResult(string path, FileType type) :
		this(new List<string>(), new FilePath(path), type) {}

	public void AddContainer(string container)
	{
		Containers.Add(container);
	}

	public int CompareByPath(FileResult other, bool caseInsensitive)
	{
		// var cmp = Settings.SortCaseInsensitive ?
		// 	StringComparison.InvariantCultureIgnoreCase :
		// 	StringComparison.InvariantCulture;
		var cmp = caseInsensitive ?
			StringComparison.OrdinalIgnoreCase :
			StringComparison.Ordinal;
		var dirNameCmp = string.Compare(FilePath.Parent?.ToString(), other.FilePath.Parent?.ToString(), cmp);
		return dirNameCmp == 0 ? string.Compare(FilePath.Name, other.FilePath.Name, cmp) : dirNameCmp;
	}

	public int CompareByName(FileResult other, bool caseInsensitive)
	{
		// var cmp = Settings.SortCaseInsensitive ?
		// 	StringComparison.InvariantCultureIgnoreCase :
		// 	StringComparison.InvariantCulture;
		var cmp = caseInsensitive ?
			StringComparison.OrdinalIgnoreCase :
			StringComparison.Ordinal;
		var fileNameCmp = string.Compare(FilePath.Name, other.FilePath.Name, cmp);
		return fileNameCmp == 0 ? string.Compare(FilePath.Parent?.ToString(), other.FilePath.Parent?.ToString(), cmp) : fileNameCmp;
	}

	public int CompareBySize(FileResult other, bool caseInsensitive)
	{
		return FilePath.Length == other.FilePath.Length ? CompareByPath(other, caseInsensitive) : FilePath.Length.CompareTo(other.FilePath.Length);
	}

	public int CompareByType(FileResult other, bool caseInsensitive)
	{
		return (int) Type == (int) other.Type ? CompareByPath(other, caseInsensitive) : ((int) Type).CompareTo((int) other.Type);
	}

	public int CompareByLastMod(FileResult other, bool caseInsensitive)
	{
		return FilePath.LastWriteTimeUtc == other.FilePath.LastWriteTimeUtc ? CompareByPath(other, caseInsensitive) : FilePath.LastWriteTimeUtc.CompareTo(other.FilePath.LastWriteTimeUtc);
	}

	public override string ToString()
	{
		var sb = new StringBuilder();
		if (Containers.Count > 0)
		{
			for (var i = 0; i < Containers.Count; i++)
			{
				if (i > 0) sb.Append(ContainerSeparator);
				sb.Append(Containers[i]);
			}
			sb.Append(ContainerSeparator);
		}
		sb.Append(PathAndName);
		return sb.ToString();
	}
}
