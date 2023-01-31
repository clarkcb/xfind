namespace CsFindLib;

public enum SortBy
{
	FilePath,
	FileName,
	FileSize,
	FileType,
	LastMod
}

public static class SortByUtil
{
	public static SortBy GetSortByFromName(string sortByName)
	{
		return sortByName.ToUpper() switch
		{
			"NAME" => SortBy.FileName,
			"SIZE" => SortBy.FileSize,
			"TYPE" => SortBy.FileType,
			"LASTMOD" => SortBy.LastMod,
			_ => SortBy.FilePath
		};
	}

	public static string GetNameFromSortBy(SortBy sortBy)
	{
		return sortBy switch
		{
			SortBy.FileName => "NAME",
			SortBy.FileSize => "SIZE",
			SortBy.FileType => "TYPE",
			SortBy.LastMod => "LASTMOD",
			_ => "PATH"
		};
	}
}
