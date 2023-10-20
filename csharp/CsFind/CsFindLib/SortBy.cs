namespace CsFindLib;

public enum SortBy
{
	FilePath,
	FileName,
	FileSize,
	FileType,
	LastMod,
	MimeType
}

public static class SortByUtil
{
	public static SortBy GetSortByFromName(string sortByName)
	{
		return sortByName.ToUpper() switch
		{
			"filename" => SortBy.FileName,
			"name" => SortBy.FileName,
			"filesize" => SortBy.FileSize,
			"size" => SortBy.FileSize,
			"filetype" => SortBy.FileType,
			"type" => SortBy.FileType,
			"lastmod" => SortBy.LastMod,
			"mimetype" => SortBy.MimeType,
			"mime" => SortBy.MimeType,
			_ => SortBy.FilePath
		};
	}

	public static string GetNameFromSortBy(SortBy sortBy)
	{
		return sortBy switch
		{
			SortBy.FileName => "filename",
			SortBy.FileSize => "filesize",
			SortBy.FileType => "filetype",
			SortBy.LastMod => "lastmod",
			SortBy.MimeType => "mimetype",
			_ => "filepath"
		};
	}
}
