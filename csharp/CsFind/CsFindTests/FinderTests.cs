using System;
using System.IO;
using System.Linq;
using CsFindLib;
using NUnit.Framework;

namespace CsFindTests;

[TestFixture]
class FinderTests
{
	private readonly FileTypes _fileTypes = new();

	private static string GetXfindPath()
	{
		var xfindPath = Environment.GetEnvironmentVariable("XFIND_PATH");
		if (xfindPath == null)
		{
			xfindPath = Path.Join(FileUtil.GetHomePath(), "src", "xfind");
		}
		return xfindPath;
	}

	private static string GetCsFindPath()
	{
		return Path.Join(GetXfindPath(), "csharp", "CsFind");
	}

	private static FindSettings GetSettings()
	{
		var settings = new FindSettings();
		settings.AddPath(".");
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
		Assert.That(finder.IsMatchingDirectory(new FilePath(".")));
	}

	[Test]
	public void TestIsMatchingDirectory_DoubleDot_True()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new FilePath("..")));
	}

	[Test]
	public void TestIsMatchingDirectory_IsHidden_False()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new FilePath(".git")), Is.False);
	}

	[Test]
	public void TestIsMatchingDirectory_IsHiddenIncludeHidden_True()
	{
		var settings = GetSettings();
		settings.IncludeHidden = true;
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new FilePath(".git")));
	}

	[Test]
	public void TestIsMatchingDirectory_NoPatterns_True()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new FilePath("/Users")));
	}

	[Test]
	public void TestIsMatchingDirectory_MatchesInPattern_True()
	{
		var settings = GetSettings();
		settings.AddInDirPattern("Find");
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new FilePath("CsFind")));
	}

	[Test]
	public void TestIsMatchingDirectory_MatchesOutPattern_False()
	{
		var settings = GetSettings();
		settings.AddOutDirPattern("Find");
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new FilePath("CsFind")), Is.False);
	}

	[Test]
	public void TestIsMatchingDirectory_DoesNotMatchInPattern_False()
	{
		var settings = GetSettings();
		settings.AddInDirPattern("FindFiles");
		var finder = new Finder(settings);
		Assert.That(finder.IsMatchingDirectory(new FilePath("CsFind")), Is.False);
	}

	[Test]
	public void TestIsMatchingDirectory_DoesNotMatchOutPattern_True()
	{
		var settings = GetSettings();
		settings.AddOutDirPattern("FindFiles");
		var finder = new Finder(settings);
		var dir = new FilePath(new DirectoryInfo("CsFind"));
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
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr));
	}

	[Test]
	public void TestIsMatchingFile_MatchesInExtension_True()
	{
		var settings = GetSettings();
		settings.AddInExtension("cs");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr));
	}

	[Test]
	public void TestIsMatchingFile_DoesNotMatchInExtension_False()
	{
		var settings = GetSettings();
		settings.AddInExtension("java");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr), Is.False);
	}


	[Test]
	public void TestIsMatchingFile_MatchesOutExtension_False()
	{
		var settings = GetSettings();
		settings.AddOutExtension("cs");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr), Is.False);
	}

	[Test]
	public void TestIsMatchingFile_DoesNotMatchOutExtension_True()
	{
		var settings = GetSettings();
		settings.AddOutExtension("java");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr));
	}

	[Test]
	public void TestIsMatchingFile_MatchesInPattern_True()
	{
		var settings = GetSettings();
		settings.AddInFilePattern("Find");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "Finder.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr));
	}

	[Test]
	public void TestIsMatchingFile_DoesNotMatchInPattern_False()
	{
		var settings = GetSettings();
		settings.AddInFilePattern("Find");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr), Is.False);
	}

	[Test]
	public void TestIsMatchingFile_MatchesOutPattern_False()
	{
		var settings = GetSettings();
		settings.AddOutFilePattern("Find");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "Finder.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr), Is.False);
	}

	[Test]
	public void TestIsMatchingFile_DoesNotMatchOutPattern_True()
	{
		var settings = GetSettings();
		settings.AddOutFilePattern("Find");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingFileResult(fr));
	}


	/*************************************************************
	 * IsMatchingArchiveFile tests
	*************************************************************/

	[Test]
	public void TestIsMatchingArchiveFile_NoExtensionsNoPatterns_True()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr));
	}

	[Test]
	public void TestIsMatchingArchiveFile_MatchesInExtension_True()
	{
		var settings = GetSettings();
		settings.AddInArchiveExtension("zip");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr));
	}

	[Test]
	public void TestIsMatchingArchiveFile_DoesNotMatchInExtension_False()
	{
		var settings = GetSettings();
		settings.AddInArchiveExtension("gz");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr), Is.False);
	}


	[Test]
	public void TestIsMatchingArchiveFile_MatchesOutExtension_False()
	{
		var settings = GetSettings();
		settings.AddOutArchiveExtension("zip");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr), Is.False);
	}

	[Test]
	public void TestIsMatchingArchiveFile_DoesNotMatchOutExtension_True()
	{
		var settings = GetSettings();
		settings.AddOutArchiveExtension("gz");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr));
	}

	[Test]
	public void TestIsMatchingArchiveFile_MatchesInPattern_True()
	{
		var settings = GetSettings();
		settings.AddInArchiveFilePattern("arch");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr));
	}

	[Test]
	public void TestIsMatchingArchiveFile_DoesNotMatchInPattern_False()
	{
		var settings = GetSettings();
		settings.AddInArchiveFilePattern("archives");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr), Is.False);
	}

	[Test]
	public void TestIsMatchingArchiveFile_MatchesOutPattern_False()
	{
		var settings = GetSettings();
		settings.AddOutArchiveFilePattern("arch");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr), Is.False);
	}

	[Test]
	public void TestIsMatchingArchiveFile_DoesNotMatchOutPattern_True()
	{
		var settings = GetSettings();
		settings.AddOutArchiveFilePattern("archives");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		var fr = new FileResult(filePath, _fileTypes.GetFileType(filePath));
		Assert.That(finder.IsMatchingArchiveFileResult(fr));
	}


	/*************************************************************
	 * FilterToFileResult tests
	*************************************************************/

	[Test]
	public void TestFilterToFileResult_IsHidden_IsNull()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), ".gitignore"));
		Assert.That(finder.FilterToFileResult(filePath), Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_IsHiddenIncludeHidden_NotNull()
	{
		var settings = GetSettings();
		settings.IncludeHidden = true;
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), ".gitignore"));
		Assert.That(finder.FilterToFileResult(filePath), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_ArchiveExcludeArchives_IsNull()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		Assert.That(finder.FilterToFileResult(filePath), Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_ArchiveIncludeArchives_NotNull()
	{
		var settings = GetSettings();
		settings.IncludeArchives = true;
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		Assert.That(finder.FilterToFileResult(filePath), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_IsArchiveFindFile_NotNull()
	{
		var settings = GetSettings();
		settings.IncludeArchives = true;
		settings.AddInArchiveExtension("zip");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		Assert.That(finder.FilterToFileResult(filePath), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_NotIsArchiveFindFile_IsNull()
	{
		var settings = GetSettings();
		settings.AddOutExtension("zip");
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		Assert.That(finder.FilterToFileResult(filePath), Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_ArchiveFileArchivesOnly_NotNull()
	{
		var settings = GetSettings();
		settings.ArchivesOnly = true;
		var finder = new Finder(settings);
		var filePath = new FilePath("archive.zip");
		Assert.That(finder.FilterToFileResult(filePath), !Is.Null);
	}


	[Test]
	public void TestFilterToFileResult_NoExtensionsNoPatterns_NotNull()
	{
		var settings = GetSettings();
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		Assert.That(finder.FilterToFileResult(filePath), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_IsFindFile_NotNull()
	{
		var settings = GetSettings();
		settings.AddInExtension("cs");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		Assert.That(finder.FilterToFileResult(filePath), !Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_NotIsFindFile_IsNull()
	{
		var settings = GetSettings();
		settings.AddOutExtension("cs");
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		Assert.That(finder.FilterToFileResult(filePath), Is.Null);
	}

	[Test]
	public void TestFilterToFileResult_NonArchiveFileArchivesOnly_IsNull()
	{
		var settings = GetSettings();
		settings.ArchivesOnly = true;
		var finder = new Finder(settings);
		var filePath = new FilePath(Path.Join(GetCsFindPath(), "CsFindLib", "FileUtil.cs"));
		Assert.That(finder.FilterToFileResult(filePath), Is.Null);
	}


	/*************************************************************
	 * FollowSymlinks tests
	 *************************************************************/

	[Test]
	public void TestFollowSymlinks_Default_Excluded()
	{
		var settings = new FindSettings();
		settings.AddPath(GetBinPath());
		var finder = new Finder(settings);
		var fileResults = finder.Find().ToList();
		Assert.That(fileResults.Count, Is.LessThan(4));
	}

	[Test]
	public void TestFollowSymlinks_FollowSymlinks_Included()
	{
		var settings = new FindSettings();
		settings.AddPath(GetBinPath());
		settings.FollowSymlinks = true;
		var finder = new Finder(settings);
		var fileResults = finder.Find().ToList();
		Assert.That(fileResults.Count, Is.EqualTo(0).Or.GreaterThan(2));
	}

	[Test]
	public void TestFollowSymlinks_NoFollowSymlinks_Excluded()
	{
		var settings = new FindSettings();
		settings.AddPath(GetBinPath());
		settings.FollowSymlinks = false;
		var finder = new Finder(settings);
		var fileResults = finder.Find().ToList();
		Assert.That(fileResults.Count, Is.LessThan(4));
	}
}
