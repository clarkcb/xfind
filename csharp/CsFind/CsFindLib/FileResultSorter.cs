using System;
using System.Collections.Generic;

namespace CsFindLib;

public class FileResultSorter(FindSettings settings)
{
    private FindSettings Settings { get; } = settings;

    private Comparison<FileResult> GetFileResultsComparison()
    {
        if (Settings.SortDescending)
        {
            return Settings.SortBy switch
            {
                SortBy.FileName => (fr1, fr2) => fr2.CompareByName(fr1, Settings.SortCaseInsensitive),
                SortBy.FileSize => (fr1, fr2) => fr2.CompareBySize(fr1, Settings.SortCaseInsensitive),
                SortBy.FileType => (fr1, fr2) => fr2.CompareByType(fr1, Settings.SortCaseInsensitive),
                SortBy.LastMod => (fr1, fr2) => fr2.CompareByLastMod(fr1, Settings.SortCaseInsensitive),
                _ => (fr1, fr2) => fr2.CompareByPath(fr1, Settings.SortCaseInsensitive)
            };
        }

        return Settings.SortBy switch
        {
            SortBy.FileName => (fr1, fr2) => fr1.CompareByName(fr2, Settings.SortCaseInsensitive),
            SortBy.FileSize => (fr1, fr2) => fr1.CompareBySize(fr2, Settings.SortCaseInsensitive),
            SortBy.FileType => (fr1, fr2) => fr1.CompareByType(fr2, Settings.SortCaseInsensitive),
            SortBy.LastMod => (fr1, fr2) => fr1.CompareByLastMod(fr2, Settings.SortCaseInsensitive),
            _ => (fr1, fr2) => fr1.CompareByPath(fr2, Settings.SortCaseInsensitive)
        };
    }

    public void Sort(List<FileResult> fileResults)
    {
        var comparison = GetFileResultsComparison();
        fileResults.Sort(comparison);
    }
}
