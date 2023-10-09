using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace CsFindLib;

public class FileResult
{
	public const string ContainerSeparator = "!";

	public IList<string> Containers { get; }
	public FileInfo File { get; }
	public FileType Type { get; }
	public string MimeType { get; }

	public string FullName => ToString();

	public string PathAndName => File.ToString();

	public FileResult(FileInfo fileInfo, FileType fileType) :
		this(new List<string>(), fileInfo, fileType, "") {}

	public FileResult(string path, string fileName, FileType type) :
		this(new List<string>(), new FileInfo(FileUtil.JoinPath(path, fileName)), type, "") {}

	public FileResult(FileInfo fileInfo, FileType type, string mimeType) :
		this(new List<string>(), fileInfo, type, mimeType) {}
	public FileResult(string path, string fileName, FileType type, string mimeType) :
		this(new List<string>(), new FileInfo(FileUtil.JoinPath(path, fileName)), type, mimeType) {}

	public FileResult(IList<string> containers, FileInfo file, FileType fileType, string mimeType)
	{
		Containers = containers;
		File = file;
		Type = fileType;
		MimeType = mimeType;
	}

	public void AddContainer(string container)
	{
		Containers.Add(container);
	}

	public FileInfo ToFileInfo()
	{
		return new FileInfo(PathAndName);
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
		if (MimeType != "")
		{
			sb.Append(" (").Append(MimeType).Append(')');
		}
		return sb.ToString();
	}

	public static int Compare(FileResult? sf1, FileResult? sf2)
	{
		if (sf1 is null && sf2 is null)
			return 0;
		if (sf1 is null)
			return -1;
		if (sf2 is null)
			return 1;

		if (sf1.File.Directory != null && sf2.File.Directory != null)
		{
			var pathCmp = string.Compare(sf1.File.Directory.ToString().ToUpperInvariant(),
				sf2.File.Directory.ToString().ToUpperInvariant(), StringComparison.Ordinal);
			if (pathCmp != 0)
			{
				return pathCmp;
			}
		}
		return string.Compare(sf1.File.Name.ToUpperInvariant(),
			sf2.File.Name.ToUpperInvariant(), StringComparison.Ordinal);
	}
}

public class FindFilesComparer : IComparer<FileResult>
{
	public int Compare(FileResult? sf1, FileResult? sf2)
	{
		return FileResult.Compare(sf1, sf2);
	}
}
