using System;
using System.Collections.Generic;
using System.Linq;

namespace CsFindLib;

using System.IO;

public class FilePath
{
    private DirectoryInfo? _dir;
    private FileInfo? _file;
    private string? _parentPath;

    public FileSystemInfo File
    {
        get
        {
            if (_dir != null) return _dir;
            return _file!;
        }
        private init
        {
            if (value is DirectoryInfo dir) { _dir = dir; }
            else { _file = value as FileInfo; }
        }
    }

    public string Path { get; }

    public FilePath(string path)
    {
        Path = path;
        if (!string.IsNullOrWhiteSpace(path))
        {
            _parentPath = System.IO.Path.GetDirectoryName(path);
            var expanded = FileUtil.ExpandPath(path);
            if (Directory.Exists(expanded))
            {
                File = new DirectoryInfo(expanded);
            } else
            {
                File = new FileInfo(expanded);
            }
        }
    }

    public FilePath(FileSystemInfo file)
    {
        File = file;
        Path = file.ToString();
        _parentPath = System.IO.Path.GetDirectoryName(Path);
    }

    public FilePath? Parent => _parentPath != null ? new FilePath(_parentPath) : null;

    public string Name => _dir != null ? _dir.Name : _file!.Name;

    public string Extension
    {
        get
        {
            if (_dir != null) return "";
            return _file!.Extension;
        }
    }

    public DateTime LastWriteTimeUtc => _dir?.LastWriteTimeUtc ?? _file!.LastWriteTimeUtc;

    public long Length => _dir != null ? 0 : _file!.Length;

    public bool Exists => File.Exists;

    public bool IsDirectory => File is DirectoryInfo;

    public bool IsFile => File is FileInfo;

    public bool IsSymlink => File.Exists && File.Attributes.HasFlag(FileAttributes.ReparsePoint);

    public IEnumerable<FilePath> EnumerateDirectories()
    {
        if (IsDirectory)
        {
            return _dir!.EnumerateDirectories()
                .Select(f => f.Name)
                .Select(f => System.IO.Path.Join(Path, f))
                .Select(f => new FilePath(f));
        }

        return [];
    }

    public IEnumerable<FilePath> EnumerateFiles(string searchPattern, EnumerationOptions options)
    {
        if (IsDirectory)
        {
            return _dir!.EnumerateFiles(searchPattern, options)
                .Select(f => f.Name)
                .Select(f => System.IO.Path.Join(Path, f))
                .Select(f => new FilePath(f));
        }

        return [];
    }

    public override string ToString()
    {
        // This is temporary to ensure benchmark output matching with other language versions
        if (Path.StartsWith('~')) return File.ToString();
        return Path;
    }
}

public class FilePathComparer : IEqualityComparer<FilePath>
{
    public bool Equals(FilePath? fp1, FilePath? fp2)
    {
        return StringComparer.InvariantCultureIgnoreCase
            .Equals(fp1?.Path, fp2?.Path);
    }

    public int GetHashCode(FilePath filePath)
    {
        return StringComparer.InvariantCultureIgnoreCase
            .GetHashCode(filePath.Path);
    }
}
