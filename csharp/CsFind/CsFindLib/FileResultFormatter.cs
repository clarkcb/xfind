using System;

namespace CsFindLib;

public class FileResultFormatter
{
    private FindSettings Settings { get; }
    private Func<FilePath, string> FormatDirPathFunc { get; }
    private Func<string, string> FormatFileNameFunc { get; }

    public FileResultFormatter(FindSettings settings)
    {
        Settings = settings;
        if (settings is { Colorize: true, InDirPatterns.Count: > 0 })
        {
            FormatDirPathFunc = FormatDirPathWithColor;
        }
        else
        {
            FormatDirPathFunc = dirPath => dirPath.ToString();
        }
        if (settings.Colorize && (settings.InExtensions.Count > 0 || settings.InFilePatterns.Count > 0))
        {
            FormatFileNameFunc = FormatFileNameWithColor;
        }
        else
        {
            FormatFileNameFunc = fileName => fileName;
        }
    }

    public static string Colorize(string s, int matchStartIndex, int matchEndIndex)
    {
        var prefix = "";
        if (matchStartIndex > 0)
        {
            prefix = s[..matchStartIndex];
        }
        var suffix = "";
        if (matchEndIndex < s.Length)
        {
            suffix = s[matchEndIndex..];
        }
        var matchLength = matchEndIndex - matchStartIndex;
        return prefix +
               ConsoleColor.Green + 
               s.Substring(matchStartIndex, matchLength) +
               ConsoleColor.Reset + 
               suffix;
    }

    private string FormatDirPathWithColor(FilePath dirPath)
    {
        var formattedDirPath = dirPath.ToString();
        foreach (var p in Settings.InDirPatterns)
        {
            var m = p.Match(formattedDirPath);
            if (m.Success)
            {
                formattedDirPath = Colorize(formattedDirPath, m.Index, m.Index + m.Length);
                break;
            }
        }
        return formattedDirPath;
    }
    
    public string FormatDirPath(FilePath dirPath) => FormatDirPathFunc(dirPath);

    private string FormatFileNameWithColor(string fileName)
    {
        var formattedFileName = fileName;
        foreach (var p in Settings.InFilePatterns)
        {
            var m = p.Match(formattedFileName);
            if (m.Success)
            {
                formattedFileName = Colorize(formattedFileName, m.Index, m.Index + m.Length);
                break;
            }
        }
        if (Settings.InExtensions.Count > 0)
        {
            var idx = formattedFileName.LastIndexOf('.');
            if (idx > 0 && idx < formattedFileName.Length - 1)
            {
                formattedFileName = Colorize(formattedFileName, idx + 1,
                    formattedFileName.Length);
            }
        }
        return formattedFileName;
    }

    public string FormatFileName(string fileName) => FormatFileNameFunc(fileName);

    public string FormatFilePath(FilePath filePath)
    {
        var parent = ".";
        if (filePath.Parent != null)
        {
            parent = FormatDirPath(filePath.Parent!);
        }
        var fileName = FormatFileName(filePath.Name);
        return System.IO.Path.Join(parent, fileName);
    }

    public string FormatFileResult(FileResult fileResult)
    {
        return FormatFilePath(fileResult.FilePath);
    }
}