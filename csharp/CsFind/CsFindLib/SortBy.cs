namespace CsFindLib;

public enum SortBy
{
	FilePath,
	FileName,
	FileType
}

public static class SortByUtil
{
	public static SortBy GetSortByFromName(string sortByName)
	{
		return sortByName.ToUpper() switch
		{
			"NAME" => SortBy.FileName,
			"TYPE" => SortBy.FileType,
			_ => SortBy.FilePath
		};
	}

	public static string GetNameFromSortBy(SortBy sortBy)
	{
		return sortBy switch
		{
			SortBy.FileName => "NAME",
			SortBy.FileType => "TYPE",
			_ => "PATH"
		};
	}
}
