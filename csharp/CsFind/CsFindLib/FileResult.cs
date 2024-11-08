using System.Collections.Generic;
using System.Text;

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
