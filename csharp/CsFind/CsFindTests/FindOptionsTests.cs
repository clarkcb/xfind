using System.Collections.Generic;
using System.IO;
using System.Linq;
using NUnit.Framework;
using CsFind;

namespace CsFindTests
{
	[TestFixture]
	public class FindOptionsTests
	{
		private readonly FindOptions _findOptions = new FindOptions();

		[Test]
		public void SettingsFromArgs_NoArgs_HasDefaultValues()
		{
			var settings = _findOptions.SettingsFromArgs(new List<string>());
			Assert.IsFalse(settings.ArchivesOnly);
			Assert.IsTrue(settings.Colorize);
			Assert.IsFalse(settings.Debug);
			Assert.IsTrue(settings.ExcludeHidden);
			Assert.IsFalse(settings.FirstMatch);
			Assert.AreEqual(0, settings.LinesAfter);
			Assert.AreEqual(0, settings.LinesBefore);
			Assert.IsFalse(settings.ListDirs);
			Assert.IsFalse(settings.ListFiles);
			Assert.IsFalse(settings.ListLines);
			Assert.AreEqual(150, settings.MaxLineLength);
			Assert.IsFalse(settings.MultiLineFind);
			Assert.IsTrue(settings.PrintResults);
			Assert.IsFalse(settings.PrintUsage);
			Assert.IsFalse(settings.PrintVersion);
			Assert.IsTrue(settings.Recursive);
			Assert.IsFalse(settings.FindArchives);
			Assert.AreEqual(null, settings.StartPath);
			Assert.IsFalse(settings.UniqueLines);
			Assert.IsFalse(settings.Verbose);
		}

		[Test]
		public void SettingsFromArgs_ValidArgs_HasArgValues()
		{
			var args = new List<string>() { "-x", "cs", "-s", "Find", "." };
			var settings = _findOptions.SettingsFromArgs(args);
			var startInfo = new DirectoryInfo(".");
			Assert.AreEqual(startInfo.ToString(), settings.StartPath);
			Assert.AreEqual(1, settings.InExtensions.Count);
			Assert.IsTrue(settings.InExtensions.Contains(".cs"));
			Assert.AreEqual(1, settings.FindPatterns.Count);
			Assert.IsTrue(settings.FindPatterns.First().ToString() == "Find");
		}

		[Test]
		public void SettingsFromArgs_InValidArgs_ThrowsFindException()
		{
			var args = new List<string>() { "-x", "cs", "-s", "Find", ".", "-Q" };
			var ex = Assert.Throws<FindException>(() => _findOptions.SettingsFromArgs(args));
			Assert.AreEqual("Invalid option: Q", ex.Message);
		}
		
		[Test]
		public void SettingsFromJson_EqualsExpected()
		{
			var json = @"{
  ""startpath"": ""~/src/xfind/"", 
  ""in-ext"": [""js"", ""ts""],
  ""out-dirpattern"": ""node_module"",
  ""out-filepattern"": [""temp""],
  ""findpattern"": ""Finder"",
  ""linesbefore"": 2,
  ""linesafter"": 2,
  ""debug"": true,
  ""allmatches"": false,
  ""includehidden"": false
}";
			var settings = new FindSettings();
			FindOptions.SettingsFromJson(json, settings);

			Assert.AreEqual("~/src/xfind/", settings.StartPath);

			Assert.AreEqual(2, settings.InExtensions.Count);
			Assert.True(settings.InExtensions.Contains(".js"));
			Assert.True(settings.InExtensions.Contains(".ts"));

			Assert.AreEqual(1, settings.OutDirPatterns.Count);
			Assert.AreEqual("node_module", settings.OutDirPatterns.First().ToString());

			Assert.AreEqual(1, settings.OutFilePatterns.Count);
			Assert.AreEqual("temp", settings.OutFilePatterns.First().ToString());

			Assert.AreEqual(1, settings.FindPatterns.Count);
			Assert.AreEqual("Finder", settings.FindPatterns.First().ToString());

			Assert.AreEqual(2, settings.LinesBefore);
			Assert.AreEqual(2, settings.LinesAfter);

			Assert.True(settings.Debug);
			Assert.True(settings.FirstMatch);
			Assert.True(settings.ExcludeHidden);
		}
	}
}
