﻿using System;
using System.IO;
using CsSearch;
using NUnit.Framework;

namespace CsSearchTests
{
	[TestFixture]
	class SearcherTests
	{
		private static SearchSettings GetSettings()
		{
			var settings = new SearchSettings();
			settings.StartPath = ".";
			settings.AddSearchPattern("Searcher");
			return settings;
		}

		/*************************************************************
		 * IsSearchDirectory tests
		*************************************************************/
		[Test]
		public void TestIsSearchDirectory_SingleDot_True()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			Assert.True(searcher.IsSearchDirectory(new DirectoryInfo(".")));
		}

		[Test]
		public void TestIsSearchDirectory_DoubleDot_True()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			Assert.True(searcher.IsSearchDirectory(new DirectoryInfo("..")));
		}

		[Test]
		public void TestIsSearchDirectory_IsHidden_False()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			Assert.False(searcher.IsSearchDirectory(new DirectoryInfo(".git")));
		}

		[Test]
		public void TestIsSearchDirectory_IsHiddenIncludeHidden_True()
		{
			var settings = GetSettings();
			settings.ExcludeHidden = false;
			var searcher = new Searcher(settings);
			Assert.True(searcher.IsSearchDirectory(new DirectoryInfo(".git")));
		}

		[Test]
		public void TestIsSearchDirectory_NoPatterns_True()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			Assert.True(searcher.IsSearchDirectory(new DirectoryInfo("/Users")));
		}

		[Test]
		public void TestIsSearchDirectory_MatchesInPattern_True()
		{
			var settings = GetSettings();
			settings.AddInDirPattern("Search");
			var searcher = new Searcher(settings);
			Assert.True(searcher.IsSearchDirectory(new DirectoryInfo("CsSearch")));
		}

		[Test]
		public void TestIsSearchDirectory_MatchesOutPattern_False()
		{
			var settings = GetSettings();
			settings.AddOutDirPattern("Search");
			var searcher = new Searcher(settings);
			Assert.False(searcher.IsSearchDirectory(new DirectoryInfo("CsSearch")));
		}

		[Test]
		public void TestIsSearchDirectory_DoesNotMatchInPattern_False()
		{
			var settings = GetSettings();
			settings.AddInDirPattern("SearchFiles");
			var searcher = new Searcher(settings);
			Assert.False(searcher.IsSearchDirectory(new DirectoryInfo("CsSearch")));
		}

		[Test]
		public void TestIsSearchDirectory_DoesNotMatchOutPattern_True()
		{
			var settings = GetSettings();
			settings.AddOutDirPattern("SearchFiles");
			var searcher = new Searcher(settings);
			var dir = new DirectoryInfo("CsSearch");
			Assert.True(searcher.IsSearchDirectory(dir));
		}


		/*************************************************************
		 * IsSearchFile tests
		*************************************************************/

		[Test]
		public void TestIsSearchFile_NoExtensionsNoPatterns_True()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.True(searcher.IsSearchFile(file));
		}

		[Test]
		public void TestIsSearchFile_MatchesInExtension_True()
		{
			var settings = GetSettings();
			settings.AddInExtension("cs");
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.True(searcher.IsSearchFile(file));
		}

		[Test]
		public void TestIsSearchFile_DoesNotMatchInExtension_False()
		{
			var settings = GetSettings();
			settings.AddInExtension("java");
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.False(searcher.IsSearchFile(file));
		}


		[Test]
		public void TestIsSearchFile_MatchesOutExtension_False()
		{
			var settings = GetSettings();
			settings.AddOutExtension("cs");
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.False(searcher.IsSearchFile(file));
		}

		[Test]
		public void TestIsSearchFile_DoesNotMatchOutExtension_True()
		{
			var settings = GetSettings();
			settings.AddOutExtension("java");
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.True(searcher.IsSearchFile(file));
		}

		[Test]
		public void TestIsSearchFile_MatchesInPattern_True()
		{
			var settings = GetSettings();
			settings.AddInFilePattern("Search");
			var searcher = new Searcher(settings);
			var file = new FileInfo("Searcher.cs");
			Assert.True(searcher.IsSearchFile(file));
		}

		[Test]
		public void TestIsSearchFile_DoesNotMatchInPattern_False()
		{
			var settings = GetSettings();
			settings.AddInFilePattern("Search");
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.False(searcher.IsSearchFile(file));
		}

		[Test]
		public void TestIsSearchFile_MatchesOutPattern_False()
		{
			var settings = GetSettings();
			settings.AddOutFilePattern("Search");
			var searcher = new Searcher(settings);
			var file = new FileInfo("Searcher.cs");
			Assert.False(searcher.IsSearchFile(file));
		}

		[Test]
		public void TestIsSearchFile_DoesNotMatchOutPattern_True()
		{
			var settings = GetSettings();
			settings.AddOutFilePattern("Search");
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.True(searcher.IsSearchFile(file));
		}


		/*************************************************************
		 * IsArchiveSearchFile tests
		*************************************************************/

		[Test]
		public void TestIsArchiveSearchFile_NoExtensionsNoPatterns_True()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.True(searcher.IsArchiveSearchFile(file));
		}

		[Test]
		public void TestIsArchiveSearchFile_MatchesInExtension_True()
		{
			var settings = GetSettings();
			settings.AddInArchiveExtension("zip");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.True(searcher.IsArchiveSearchFile(file));
		}

		[Test]
		public void TestIsArchiveSearchFile_DoesNotMatchInExtension_False()
		{
			var settings = GetSettings();
			settings.AddInArchiveExtension("gz");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.False(searcher.IsArchiveSearchFile(file));
		}


		[Test]
		public void TestIsArchiveSearchFile_MatchesOutExtension_False()
		{
			var settings = GetSettings();
			settings.AddOutArchiveExtension("zip");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.False(searcher.IsArchiveSearchFile(file));
		}

		[Test]
		public void TestIsArchiveSearchFile_DoesNotMatchOutExtension_True()
		{
			var settings = GetSettings();
			settings.AddOutArchiveExtension("gz");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.True(searcher.IsArchiveSearchFile(file));
		}

		[Test]
		public void TestIsArchiveSearchFile_MatchesInPattern_True()
		{
			var settings = GetSettings();
			settings.AddInArchiveFilePattern("arch");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.True(searcher.IsArchiveSearchFile(file));
		}

		[Test]
		public void TestIsArchiveSearchFile_DoesNotMatchInPattern_False()
		{
			var settings = GetSettings();
			settings.AddInArchiveFilePattern("archives");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.False(searcher.IsArchiveSearchFile(file));
		}

		[Test]
		public void TestIsArchiveSearchFile_MatchesOutPattern_False()
		{
			var settings = GetSettings();
			settings.AddOutArchiveFilePattern("arch");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.False(searcher.IsArchiveSearchFile(file));
		}

		[Test]
		public void TestIsArchiveSearchFile_DoesNotMatchOutPattern_True()
		{
			var settings = GetSettings();
			settings.AddOutArchiveFilePattern("archives");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.True(searcher.IsArchiveSearchFile(file));
		}

		/*************************************************************
		 * FilterFile tests
		*************************************************************/

		[Test]
		public void TestFilterFile_IsHidden_False()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			var file = new FileInfo(".gitignore");
			Assert.False(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_IsHiddenIncludeHidden_True()
		{
			var settings = GetSettings();
			settings.ExcludeHidden = false;
			var searcher = new Searcher(settings);
			var file = new FileInfo(".gitignore");
			Assert.True(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_ArchiveNoSearchArchives_False()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.False(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_ArchiveSearchArchives_True()
		{
			var settings = GetSettings();
			settings.SearchArchives = true;
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.True(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_IsArchiveSearchFile_True()
		{
			var settings = GetSettings();
			settings.SearchArchives = true;
			settings.AddInArchiveExtension("zip");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.True(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_NotIsArchiveSearchFile_False()
		{
			var settings = GetSettings();
			settings.AddOutExtension("zip");
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.False(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_ArchiveFileArchivesOnly_True()
		{
			var settings = GetSettings();
			settings.ArchivesOnly = true;
			var searcher = new Searcher(settings);
			var file = new FileInfo("archive.zip");
			Assert.False(searcher.FilterFile(file));
		}


		[Test]
		public void TestFilterFile_NoExtensionsNoPatterns_True()
		{
			var settings = GetSettings();
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.True(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_IsSearchFile_True()
		{
			var settings = GetSettings();
			settings.AddInExtension("cs");
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.True(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_NotIsSearchFile_False()
		{
			var settings = GetSettings();
			settings.AddOutExtension("cs");
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.False(searcher.FilterFile(file));
		}

		[Test]
		public void TestFilterFile_NonArchiveFileArchivesOnly_False()
		{
			var settings = GetSettings();
			settings.ArchivesOnly = true;
			var searcher = new Searcher(settings);
			var file = new FileInfo("FileUtil.cs");
			Assert.False(searcher.FilterFile(file));
		}
	}
}