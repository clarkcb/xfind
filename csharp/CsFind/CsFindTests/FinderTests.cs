using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using CsFindLib;
using NUnit.Framework;

namespace CsFindTests;

[TestFixture]
class FinderTests
{
	private readonly FileTypes _fileTypes = new();

	private static string GetTestFileContent()
	{
		return EmbeddedTestResource.GetResourceFileContents("CsFindTests.Resources.testFile2.txt");
	}

	public static string GetXfindPath()
	{
		var xfindPath = Environment.GetEnvironmentVariable("XFIND_PATH");
		if (xfindPath == null)
		{
			xfindPath = Path.Join(FileUtil.GetHomePath(), "src", "xfind");
		}
		return xfindPath;
	}

	public static string GetCsFindPath()
	{
		return Path.Join(GetXfindPath(), "csharp", "CsFind");
	}

	public static IEnumerable<string> GetTestFileLines()
	{
		var testFile2Contents = GetTestFileContent();
		foreach (var line in testFile2Contents.Split(new[] { "\n", "\r" }, StringSplitOptions.None))
		{
			yield return line;
		}
	}

	private static FindSettings GetSettings()
	{
		var settings = new FindSettings();
		settings.Paths.Add(".");
		return settings;
	}

	private static string GetBinPath()
	{
		var xfindPath = Environment.GetEnvironmentVariable("XFIND_PATH");
		if (xfindPath == null)
		{
			xfindPath = Path.Join(FileUtil.GetHomePath(), "src", "xfind");
		}
		return Path.Join(xfindPath, "bin");
	}

	/*************************************************************
	 * IsMatchingDirectory tests
	*************************************************************/
	[Test]
	public void TestIsMatchingDirectory_SingleDot_True()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new DirectoryInfo(".")));
	}

	[Test]
	public void TestIsMatchingDirectory_DoubleDot_True()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new DirectoryInfo("..")));
	}

	[Test]
	public void TestIsMatchingDirectory_IsHidden_False()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new DirectoryInfo(".git")), Is.False);
	}

	[Test]
	public void TestIsMatchingDirectory_IsHiddenIncludeHidden_True()
	{
		var settings = GetSettings();
		settings.IncludeHidden = true;
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new DirectoryInfo(".git")));
	}

	[Test]
	public void TestIsMatchingDirectory_NoPatterns_True()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new DirectoryInfo("/Users")));
	}

	[Test]
	public void TestIsMatchingDirectory_MatchesInPattern_True()
	{
		var settings = GetSettings();
		settings.AddInDirPattern("Find");
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new DirectoryInfo("CsFind")));
	}

	[Test]
	public void TestIsMatchingDirectory_MatchesOutPattern_False()
	{
		var settings = GetSettings();
		settings.AddOutDirPattern("Find");
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new DirectoryInfo("CsFind")), Is.False);
	}

	[Test]
	public void TestIsMatchingDirectory_DoesNotMatchInPattern_False()
	{
		var settings = GetSettings();
		settings.AddInDirPattern("FindFiles");
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new DirectoryInfo("CsFind")), Is.False);
	}

	[Test]
	public void TestIsMatchingDirectory_DoesNotMatchOutPattern_True()
	{
		var settings = GetSettings();
		settings.AddOutDirPattern("FindFiles");
		var finder = new Finder(settings);
		var dir = new DirectoryInfo("CsFind");
		Assert.That(finder.IsMatchingDirectory(dir));
	}


	/*************************************************************
	 * IsMatchingFile tests
	*************************************************************/

	[Test]
	public void TestIsMatchingFile_NoExtensionsNoPatterns_True()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf));
	}

	[Test]
	public void TestIsMatchingFile_MatchesInExtension_True()
	{
		var settings = GetSettings();
		settings.AddInExtension("cs");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf));
	}

	[Test]
	public void TestIsMatchingFile_DoesNotMatchInExtension_False()
	{
		var settings = GetSettings();
		settings.AddInExtension("java");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf), Is.False);
	}


	[Test]
	public void TestIsMatchingFile_MatchesOutExtension_False()
	{
		var settings = GetSettings();
		settings.AddOutExtension("cs");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf), Is.False);
	}

	[Test]
	public void TestIsMatchingFile_DoesNotMatchOutExtension_True()
	{
		var settings = GetSettings();
		settings.AddOutExtension("java");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf));
	}

	[Test]
	public void TestIsMatchingFile_MatchesInPattern_True()
	{
		var settings = GetSettings();
		settings.AddInFilePattern("Find");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "Finder.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf));
	}

	[Test]
	public void TestIsMatchingFile_DoesNotMatchInPattern_False()
	{
		var settings = GetSettings();
		settings.AddInFilePattern("Find");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf), Is.False);
	}

	[Test]
	public void TestIsMatchingFile_MatchesOutPattern_False()
	{
		var settings = GetSettings();
		settings.AddOutFilePattern("Find");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "Finder.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf), Is.False);
	}

	[Test]
	public void TestIsMatchingFile_DoesNotMatchOutPattern_True()
	{
		var settings = GetSettings();
		settings.AddOutFilePattern("Find");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingFileResult(sf));
	}


	/*************************************************************
	 * IsMatchingArchiveFile tests
	*************************************************************/

	[Test]
	public void TestIsMatchingArchiveFile_NoExtensionsNoPatterns_True()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf));
	}

	[Test]
	public void TestIsMatchingArchiveFile_MatchesInExtension_True()
	{
		var settings = GetSettings();
		settings.AddInArchiveExtension("zip");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf));
	}

	[Test]
	public void TestIsMatchingArchiveFile_DoesNotMatchInExtension_False()
	{
		var settings = GetSettings();
		settings.AddInArchiveExtension("gz");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf), Is.False);
	}


	[Test]
	public void TestIsMatchingArchiveFile_MatchesOutExtension_False()
	{
		var settings = GetSettings();
		settings.AddOutArchiveExtension("zip");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf), Is.False);
	}

	[Test]
	public void TestIsMatchingArchiveFile_DoesNotMatchOutExtension_True()
	{
		var settings = GetSettings();
		settings.AddOutArchiveExtension("gz");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf));
	}

	[Test]
	public void TestIsMatchingArchiveFile_MatchesInPattern_True()
	{
		var settings = GetSettings();
		settings.AddInArchiveFilePattern("arch");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf));
	}

	[Test]
	public void TestIsMatchingArchiveFile_DoesNotMatchInPattern_False()
	{
		var settings = GetSettings();
		settings.AddInArchiveFilePattern("archives");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf), Is.False);
	}

	[Test]
	public void TestIsMatchingArchiveFile_MatchesOutPattern_False()
	{
		var settings = GetSettings();
		settings.AddOutArchiveFilePattern("arch");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf), Is.False);
	}

	[Test]
	public void TestIsMatchingArchiveFile_DoesNotMatchOutPattern_True()
	{
		var settings = GetSettings();
		settings.AddOutArchiveFilePattern("archives");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		var sf = new FileResult(file, _fileTypes.GetFileType(file));
		Assert.That(finder.IsMatchingArchiveFileResult(sf));
	}


	/*************************************************************
	 * FilterToFileResult tests
	*************************************************************/

	[Test]
	public void TestFilterToFileResult_IsHidden_IsNull()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), ".gitignore"));
		Assert.That(finder.FilterToFileResult(file), Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_IsHiddenIncludeHidden_NotNull()
	{
		var settings = GetSettings();
		settings.IncludeHidden = true;
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), ".gitignore"));
		Assert.That(finder.FilterToFileResult(file), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_ArchiveExcludeArchives_IsNull()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		Assert.That(finder.FilterToFileResult(file), Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_ArchiveIncludeArchives_NotNull()
	{
		var settings = GetSettings();
		settings.IncludeArchives = true;
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		Assert.That(finder.FilterToFileResult(file), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_IsArchiveFindFile_NotNull()
	{
		var settings = GetSettings();
		settings.IncludeArchives = true;
		settings.AddInArchiveExtension("zip");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		Assert.That(finder.FilterToFileResult(file), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_NotIsArchiveFindFile_IsNull()
	{
		var settings = GetSettings();
		settings.AddOutExtension("zip");
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		Assert.That(finder.FilterToFileResult(file), Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_ArchiveFileArchivesOnly_NotNull()
	{
		var settings = GetSettings();
		settings.ArchivesOnly = true;
		var finder = new Finder(settings);
		var file = new FileInfo("archive.zip");
		Assert.That(finder.FilterToFileResult(file), !Is.Null);
	}


	[Test]
	public void TestFilterToFileResult_NoExtensionsNoPatterns_NotNull()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		Assert.That(finder.FilterToFileResult(file), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_IsFindFile_NotNull()
	{
		var settings = GetSettings();
		settings.AddInExtension("cs");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		Assert.That(finder.FilterToFileResult(file), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_NotIsFindFile_IsNull()
	{
		var settings = GetSettings();
		settings.AddOutExtension("cs");
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		Assert.That(finder.FilterToFileResult(file), Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_NonArchiveFileArchivesOnly_IsNull()
	{
		var settings = GetSettings();
		settings.ArchivesOnly = true;
		var finder = new Finder(settings);
		var file = new FileInfo(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		Assert.That(finder.FilterToFileResult(file), Is.Null);
	}


	/*************************************************************
	 * FollowSymlinks tests
	 *************************************************************/

	[Test]
	public void TestFollowSymlinks_Default_Excluded()
	{
		var settings = new FindSettings();
		settings.Paths.Add(GetBinPath());
		var finder = new Finder(settings);
		var fileResults = finder.Find().ToList();
		Assert.That(fileResults.Count, Is.LessThan(3));
	}

	[Test]
	public void TestFollowSymlinks_FollowSymlinks_Included()
	{
		var settings = new FindSettings();
		settings.Paths.Add(GetBinPath());
		settings.FollowSymlinks = true;
		var finder = new Finder(settings);
		var fileResults = finder.Find().ToList();
		Assert.That(fileResults.Count, Is.EqualTo(0).Or.GreaterThan(2));
	}

	[Test]
	public void TestFollowSymlinks_NoFollowSymlinks_Excluded()
	{
		var settings = new FindSettings();
		settings.Paths.Add(GetBinPath());
		settings.FollowSymlinks = false;
		var finder = new Finder(settings);
		var fileResults = finder.Find().ToList();
		Assert.That(fileResults.Count, Is.LessThan(3));
	}
}
