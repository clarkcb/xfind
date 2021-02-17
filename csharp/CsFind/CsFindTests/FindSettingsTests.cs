using System.Linq;
using NUnit.Framework;
using CsFind;

namespace CsFindTests
{
	[TestFixture]
	public class FindSettingsTests
	{
		[Test]
		public void GetNewFindSettings_NoModifications_HasDefaultValues()
		{
			var settings = new FindSettings();
			Assert.IsFalse(settings.ArchivesOnly);
			Assert.IsTrue(settings.Colorize);
			Assert.IsFalse(settings.Debug);
			Assert.IsTrue(settings.ExcludeHidden);
			Assert.IsFalse(settings.FirstMatch);
			Assert.AreEqual(settings.LinesAfter, 0);
			Assert.AreEqual(settings.LinesBefore, 0);
			Assert.IsFalse(settings.ListDirs);
			Assert.IsFalse(settings.ListFiles);
			Assert.IsFalse(settings.ListLines);
			Assert.AreEqual(settings.MaxLineLength, 150);
			Assert.IsFalse(settings.MultiLineFind);
			Assert.IsFalse(settings.PrintResults);
			Assert.IsFalse(settings.PrintUsage);
			Assert.IsFalse(settings.PrintVersion);
			Assert.IsTrue(settings.Recursive);
			Assert.IsFalse(settings.FindArchives);
			Assert.IsFalse(settings.UniqueLines);
			Assert.IsFalse(settings.Verbose);
		}

		[Test]
		public void FindSettings_AddExtensions_HasExtensions()
		{
			var settings = new FindSettings();
			settings.AddInExtension("cs");
			Assert.AreEqual(settings.InExtensions.Count, 1);
			Assert.IsTrue(settings.InExtensions.Contains(".cs"));
			settings.AddInExtension("java,scala");
			Assert.AreEqual(settings.InExtensions.Count, 3);
			Assert.IsTrue(settings.InExtensions.Contains(".java"));
			Assert.IsTrue(settings.InExtensions.Contains(".scala"));
		}

		[Test]
		public void FindSettings_AddPatterns_HasPatterns()
		{
			var settings = new FindSettings();
			settings.AddFindPattern("Find");
			Assert.AreEqual(settings.FindPatterns.Count, 1);
			Assert.IsTrue(settings.FindPatterns.First().ToString() == "Find");
		}

		[Test]
		public void FindSettings_SetArchivesOnly_HasFindArchives()
		{
			var settings = new FindSettings {ArchivesOnly = true};
			Assert.IsTrue(settings.ArchivesOnly);
			Assert.IsTrue(settings.FindArchives);
		}

		[Test]
		public void FindSettings_SetDebug_HasVerbose()
		{
			var settings = new FindSettings {Debug = true};
			Assert.IsTrue(settings.Debug);
			Assert.IsTrue(settings.Verbose);
		}
	}
}
