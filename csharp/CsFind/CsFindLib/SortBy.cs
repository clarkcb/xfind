using System;

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
		return Enum.TryParse<SortBy>(sortByName, out var sortBy) ? sortBy : SortBy.FilePath;
	}

	public static string GetNameFromSortBy(SortBy sortBy)
	{
		return sortBy switch
		{
			SortBy.FileName => "filename",
			SortBy.FileSize => "filesize",
			SortBy.FileType => "filetype",
			SortBy.LastMod => "lastmod",
			_ => "filepath"
		};
	}
}
