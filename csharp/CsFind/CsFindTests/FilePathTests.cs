using System.IO;
using CsFindLib;
using NUnit.Framework;

namespace CsFindTests;

[TestFixture]
class FilePathTests
{
    [Test]
    public void FilePath_NameOnly_IsFile()
    {
        const string fileName = ".gitignore";
        var filePath = new FilePath(fileName);
        Assert.That(filePath.Exists, Is.False);
        Assert.That(filePath.IsDirectory, Is.False);
        Assert.That(filePath.IsFile, Is.True);
        Assert.That(filePath.IsSymlink, Is.False);
        Assert.That(filePath.Path, Is.EqualTo(fileName));
        Assert.That(filePath.ToString(), Is.EqualTo(fileName));
    }

    [Test]
    public void FilePath_PathToDirectory_IsDirectory()
    {
        var csFindPath = Path.Join(FileUtil.GetHomePath(), "src", "xfind", "csharp", "CsFind");
        var filePath = new FilePath(csFindPath);
        Assert.That(filePath.Exists, Is.True);
        Assert.That(filePath.IsDirectory, Is.True);
        Assert.That(filePath.IsFile, Is.False);
        Assert.That(filePath.IsSymlink, Is.False);
        Assert.That(filePath.Path, Is.EqualTo(csFindPath));
        Assert.That(filePath.ToString(), Is.EqualTo(csFindPath));
    }

    [Test]
    public void FilePath_PathToFile_IsFile()
    {
        var csFindLibPath = Path.Join(FileUtil.GetHomePath(), "src", "xfind", "csharp", "CsFind", "CsFindLib");
        var csFinderPath = Path.Join(csFindLibPath, "Finder.cs");
        var filePath = new FilePath(csFinderPath);
        Assert.That(filePath.Exists, Is.True);
        Assert.That(filePath.IsDirectory, Is.False);
        Assert.That(filePath.IsFile, Is.True);
        Assert.That(filePath.IsSymlink, Is.False);
        Assert.That(filePath.Path, Is.EqualTo(csFinderPath));
        Assert.That(filePath.ToString(), Is.EqualTo(csFinderPath));
        Assert.That(filePath.Name, Is.EqualTo("Finder.cs"));
        Assert.That(filePath.Extension, Is.EqualTo(".cs"));
        Assert.That(filePath.Parent!.ToString(), Is.EqualTo(csFindLibPath));
    }

    [Test]
    public void FilePath_PathToSymlink_IsSymlink()
    {
        var xfindBinPath = Path.Join(FileUtil.GetHomePath(), "src", "xfind", "bin");
        var csFindPath = Path.Join(xfindBinPath, "csfind");
        var filePath = new FilePath(csFindPath);
        Assert.That(filePath.Exists, Is.True);
        Assert.That(filePath.IsDirectory, Is.False);
        Assert.That(filePath.IsFile, Is.True);
        Assert.That(filePath.IsSymlink, Is.True);
        Assert.That(filePath.Path, Is.EqualTo(csFindPath));
        Assert.That(filePath.ToString(), Is.EqualTo(csFindPath));
        Assert.That(filePath.Name, Is.EqualTo("csfind"));
        Assert.That(filePath.Extension, Is.EqualTo(""));
        Assert.That(filePath.Parent!.ToString(), Is.EqualTo(xfindBinPath));
    }

    [Test]
    public void FilePath_TildePathToDirectory_IsDirectory()
    {
        const string csFindPath = "~/src/xfind/csharp/CsFind";
        var filePath = new FilePath(csFindPath);
        Assert.That(filePath.Exists, Is.True);
        Assert.That(filePath.IsDirectory, Is.True);
        Assert.That(filePath.IsFile, Is.False);
        Assert.That(filePath.IsSymlink, Is.False);
        Assert.That(filePath.Path, Is.EqualTo(csFindPath));
        Assert.That(filePath.ToString(), Is.EqualTo(csFindPath));
    }

    [Test]
    public void FilePath_TildePathToFile_IsFile()
    {
        const string csFindLibPath = "~/src/xfind/csharp/CsFind/CsFindLib";
        var csFinderPath = Path.Join(csFindLibPath, "Finder.cs");
        var filePath = new FilePath(csFinderPath);
        Assert.That(filePath.Exists, Is.True);
        Assert.That(filePath.IsDirectory, Is.False);
        Assert.That(filePath.IsFile, Is.True);
        Assert.That(filePath.IsSymlink, Is.False);
        Assert.That(filePath.Path, Is.EqualTo(csFinderPath));
        Assert.That(filePath.ToString(), Is.EqualTo(csFinderPath));
        Assert.That(filePath.Name, Is.EqualTo("Finder.cs"));
        Assert.That(filePath.Extension, Is.EqualTo(".cs"));
        Assert.That(filePath.Parent!.ToString(), Is.EqualTo(csFindLibPath));
    }

    [Test]
    public void FilePath_TildePathToSymlink_IsSymlink()
    {
        const string xfindBinPath = "~/src/xfind/bin";
        var csFindPath = Path.Join(xfindBinPath, "csfind");
        var filePath = new FilePath(csFindPath);
        Assert.That(filePath.Exists, Is.True);
        Assert.That(filePath.IsDirectory, Is.False);
        Assert.That(filePath.IsFile, Is.True);
        Assert.That(filePath.IsSymlink, Is.True);
        Assert.That(filePath.Path, Is.EqualTo(csFindPath));
        Assert.That(filePath.ToString(), Is.EqualTo(csFindPath));
        Assert.That(filePath.Name, Is.EqualTo("csfind"));
        Assert.That(filePath.Extension, Is.EqualTo(""));
        Assert.That(filePath.Parent!.ToString(), Is.EqualTo(xfindBinPath));
    }
}
