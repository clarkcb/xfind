using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace CsFindLib;

public class FindFile
{
	public static string ContainerSeparator = "!";

	public IList<string> Containers { get; private set; }
	public FileInfo File { get; private set; }
	public FileType Type { get; private set; }

	public string FullName => ToString();

	public string PathAndName => File.ToString();

	public FindFile(FileInfo fileInfo, FileType type) :
		this(new List<string>(), fileInfo, type) {}

	public FindFile(string path, string fileName, FileType type) :
		this(new List<string>(), new FileInfo(FileUtil.JoinPath(path, fileName)), type) {}

	public FindFile(IList<string> containers, FileInfo file, FileType type)
	{
		Containers = containers;
		File = file;
		Type = type;
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
		return sb.ToString();
	}

	public static int Compare(FindFile? sf1, FindFile? sf2)
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

public class FindFilesComparer : IComparer<FindFile>
{
	public int Compare(FindFile? sf1, FindFile? sf2)
	{
		return FindFile.Compare(sf1, sf2);
	}
}
