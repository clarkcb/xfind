using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using CsFind;
using NUnit.Framework;

namespace CsFindTests
{
	[TestFixture]
	class FinderTests
	{
		private readonly FileTypes _fileTypes = new FileTypes();
		
		private static string GetTestFileContent()
		{
			return EmbeddedTestResource.GetResourceFileContents("CsFindTests.Resources.testFile2.txt");
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
			var settings = new FindSettings {StartPath = "."};
			settings.AddFindPattern("Finder");
			return settings;
		}

		/*************************************************************
		 * IsFindDirectory tests
		*************************************************************/
		[Test]
		public void TestIsFindDirectory_SingleDot_True()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			Assert.True(finder.IsFindDirectory(new DirectoryInfo(".")));
		}

		[Test]
		public void TestIsFindDirectory_DoubleDot_True()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			Assert.True(finder.IsFindDirectory(new DirectoryInfo("..")));
		}

		[Test]
		public void TestIsFindDirectory_IsHidden_False()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			Assert.False(finder.IsFindDirectory(new DirectoryInfo(".git")));
		}

		[Test]
		public void TestIsFindDirectory_IsHiddenIncludeHidden_True()
		{
			var settings = GetSettings();
			settings.ExcludeHidden = false;
			var finder = new Finder(settings);
			Assert.True(finder.IsFindDirectory(new DirectoryInfo(".git")));
		}

		[Test]
		public void TestIsFindDirectory_NoPatterns_True()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			Assert.True(finder.IsFindDirectory(new DirectoryInfo("/Users")));
		}

		[Test]
		public void TestIsFindDirectory_MatchesInPattern_True()
		{
			var settings = GetSettings();
			settings.AddInDirPattern("Find");
			var finder = new Finder(settings);
			Assert.True(finder.IsFindDirectory(new DirectoryInfo("CsFind")));
		}

		[Test]
		public void TestIsFindDirectory_MatchesOutPattern_False()
		{
			var settings = GetSettings();
			settings.AddOutDirPattern("Find");
			var finder = new Finder(settings);
			Assert.False(finder.IsFindDirectory(new DirectoryInfo("CsFind")));
		}

		[Test]
		public void TestIsFindDirectory_DoesNotMatchInPattern_False()
		{
			var settings = GetSettings();
			settings.AddInDirPattern("FindFiles");
			var finder = new Finder(settings);
			Assert.False(finder.IsFindDirectory(new DirectoryInfo("CsFind")));
		}

		[Test]
		public void TestIsFindDirectory_DoesNotMatchOutPattern_True()
		{
			var settings = GetSettings();
			settings.AddOutDirPattern("FindFiles");
			var finder = new Finder(settings);
			var dir = new DirectoryInfo("CsFind");
			Assert.True(finder.IsFindDirectory(dir));
		}


		/*************************************************************
		 * IsFindFile tests
		*************************************************************/

		[Test]
		public void TestIsFindFile_NoExtensionsNoPatterns_True()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsFindFile(sf));
		}

		[Test]
		public void TestIsFindFile_MatchesInExtension_True()
		{
			var settings = GetSettings();
			settings.AddInExtension("cs");
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsFindFile(sf));
		}

		[Test]
		public void TestIsFindFile_DoesNotMatchInExtension_False()
		{
			var settings = GetSettings();
			settings.AddInExtension("java");
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.IsFindFile(sf));
		}


		[Test]
		public void TestIsFindFile_MatchesOutExtension_False()
		{
			var settings = GetSettings();
			settings.AddOutExtension("cs");
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.IsFindFile(sf));
		}

		[Test]
		public void TestIsFindFile_DoesNotMatchOutExtension_True()
		{
			var settings = GetSettings();
			settings.AddOutExtension("java");
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsFindFile(sf));
		}

		[Test]
		public void TestIsFindFile_MatchesInPattern_True()
		{
			var settings = GetSettings();
			settings.AddInFilePattern("Find");
			var finder = new Finder(settings);
			var file = new FileInfo("Finder.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsFindFile(sf));
		}

		[Test]
		public void TestIsFindFile_DoesNotMatchInPattern_False()
		{
			var settings = GetSettings();
			settings.AddInFilePattern("Find");
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.IsFindFile(sf));
		}

		[Test]
		public void TestIsFindFile_MatchesOutPattern_False()
		{
			var settings = GetSettings();
			settings.AddOutFilePattern("Find");
			var finder = new Finder(settings);
			var file = new FileInfo("Finder.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.IsFindFile(sf));
		}

		[Test]
		public void TestIsFindFile_DoesNotMatchOutPattern_True()
		{
			var settings = GetSettings();
			settings.AddOutFilePattern("Find");
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsFindFile(sf));
		}


		/*************************************************************
		 * IsArchiveFindFile tests
		*************************************************************/

		[Test]
		public void TestIsArchiveFindFile_NoExtensionsNoPatterns_True()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsArchiveFindFile(sf));
		}

		[Test]
		public void TestIsArchiveFindFile_MatchesInExtension_True()
		{
			var settings = GetSettings();
			settings.AddInArchiveExtension("zip");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsArchiveFindFile(sf));
		}

		[Test]
		public void TestIsArchiveFindFile_DoesNotMatchInExtension_False()
		{
			var settings = GetSettings();
			settings.AddInArchiveExtension("gz");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.IsArchiveFindFile(sf));
		}


		[Test]
		public void TestIsArchiveFindFile_MatchesOutExtension_False()
		{
			var settings = GetSettings();
			settings.AddOutArchiveExtension("zip");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.IsArchiveFindFile(sf));
		}

		[Test]
		public void TestIsArchiveFindFile_DoesNotMatchOutExtension_True()
		{
			var settings = GetSettings();
			settings.AddOutArchiveExtension("gz");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsArchiveFindFile(sf));
		}

		[Test]
		public void TestIsArchiveFindFile_MatchesInPattern_True()
		{
			var settings = GetSettings();
			settings.AddInArchiveFilePattern("arch");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsArchiveFindFile(sf));
		}

		[Test]
		public void TestIsArchiveFindFile_DoesNotMatchInPattern_False()
		{
			var settings = GetSettings();
			settings.AddInArchiveFilePattern("archives");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.IsArchiveFindFile(sf));
		}

		[Test]
		public void TestIsArchiveFindFile_MatchesOutPattern_False()
		{
			var settings = GetSettings();
			settings.AddOutArchiveFilePattern("arch");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.IsArchiveFindFile(sf));
		}

		[Test]
		public void TestIsArchiveFindFile_DoesNotMatchOutPattern_True()
		{
			var settings = GetSettings();
			settings.AddOutArchiveFilePattern("archives");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.IsArchiveFindFile(sf));
		}

		/*************************************************************
		 * FilterFile tests
		*************************************************************/

		[Test]
		public void TestFilterFile_IsHidden_False()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			var file = new FileInfo(".gitignore");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_IsHiddenIncludeHidden_True()
		{
			var settings = GetSettings();
			settings.ExcludeHidden = false;
			var finder = new Finder(settings);
			var file = new FileInfo(".gitignore");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_ArchiveNoFindArchives_False()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_ArchiveFindArchives_True()
		{
			var settings = GetSettings();
			settings.FindArchives = true;
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_IsArchiveFindFile_True()
		{
			var settings = GetSettings();
			settings.FindArchives = true;
			settings.AddInArchiveExtension("zip");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_NotIsArchiveFindFile_False()
		{
			var settings = GetSettings();
			settings.AddOutExtension("zip");
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_ArchiveFileArchivesOnly_True()
		{
			var settings = GetSettings();
			settings.ArchivesOnly = true;
			var finder = new Finder(settings);
			var file = new FileInfo("archive.zip");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.FilterFile(sf));
		}


		[Test]
		public void TestFilterFile_NoExtensionsNoPatterns_True()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_IsFindFile_True()
		{
			var settings = GetSettings();
			settings.AddInExtension("cs");
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.True(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_NotIsFindFile_False()
		{
			var settings = GetSettings();
			settings.AddOutExtension("cs");
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.FilterFile(sf));
		}

		[Test]
		public void TestFilterFile_NonArchiveFileArchivesOnly_False()
		{
			var settings = GetSettings();
			settings.ArchivesOnly = true;
			var finder = new Finder(settings);
			var file = new FileInfo("FileUtil.cs");
			var sf = new FindFile(file, _fileTypes.GetFileType(file));
			Assert.False(finder.FilterFile(sf));
		}


		/*************************************************************
		 * FindTextReaderLines test
		 *************************************************************/
		[Test]
		public void TestFindTextReaderLines()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			var enumerableLines = GetTestFileLines();
			var results = finder.FindLines(enumerableLines).ToList();

			Assert.True(results.Count == 2);

			var firstResult = results[0];
			const int expectedFirstLineNum = 29;
			Assert.AreEqual(expectedFirstLineNum, firstResult.LineNum);
			const int expectedFirstMatchStartIndex = 3;
			Assert.AreEqual(expectedFirstMatchStartIndex, firstResult.MatchStartIndex);
			const int expectedFirstMatchEndIndex = 11;
			Assert.AreEqual(expectedFirstMatchEndIndex, firstResult.MatchEndIndex);

			var secondResult = results[1];
			const int expectedSecondLineNum = 35;
			Assert.AreEqual(expectedSecondLineNum, secondResult.LineNum);
			const int expectedSecondMatchStartIndex = 24;
			Assert.AreEqual(expectedSecondMatchStartIndex, secondResult.MatchStartIndex);
			const int expectedSecondMatchEndIndex = 32;
			Assert.AreEqual(expectedSecondMatchEndIndex, secondResult.MatchEndIndex);
		}

		/*************************************************************
		 * FindMultiLineString test
		 *************************************************************/
		[Test]
		public void TestFindMultiLineString()
		{
			var settings = GetSettings();
			var finder = new Finder(settings);
			var contents = GetTestFileContent();
			var results = finder.FindContents(contents).ToList();

			Assert.True(results.Count == 2);

			var firstResult = results[0];
			const int expectedFirstLineNum = 29;
			Assert.AreEqual(expectedFirstLineNum, firstResult.LineNum);
			const int expectedFirstMatchStartIndex = 3;
			Assert.AreEqual(expectedFirstMatchStartIndex, firstResult.MatchStartIndex);
			const int expectedFirstMatchEndIndex = 11;
			Assert.AreEqual(expectedFirstMatchEndIndex, firstResult.MatchEndIndex);

			var secondResult = results[1];
			const int expectedSecondLineNum = 35;
			Assert.AreEqual(expectedSecondLineNum, secondResult.LineNum);
			const int expectedSecondMatchStartIndex = 24;
			Assert.AreEqual(expectedSecondMatchStartIndex, secondResult.MatchStartIndex);
			const int expectedSecondMatchEndIndex = 32;
			Assert.AreEqual(expectedSecondMatchEndIndex, secondResult.MatchEndIndex);
		}
	}
}
