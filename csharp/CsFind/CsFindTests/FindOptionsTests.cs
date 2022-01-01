using System.Collections.Generic;
using System.IO;
using System.Linq;
using NUnit.Framework;
using CsFindLib;
using FindOptions = CsFindLib.FindOptions;

namespace CsFindTests;

[TestFixture]
public class FindOptionsTests
{
	private readonly FindOptions _findOptions = new FindOptions();

	[Test]
	public void SettingsFromArgs_NoArgs_HasDefaultValues()
	{
		var settings = _findOptions.SettingsFromArgs(new List<string>());
		Assert.IsFalse(settings.ArchivesOnly);
		Assert.IsFalse(settings.Debug);
		Assert.IsTrue(settings.ExcludeHidden);
		Assert.IsFalse(settings.IncludeArchives);
		Assert.IsFalse(settings.ListDirs);
		Assert.IsTrue(settings.ListFiles);
		Assert.IsFalse(settings.PrintUsage);
		Assert.IsFalse(settings.PrintVersion);
		Assert.IsTrue(settings.Recursive);
		Assert.AreEqual(0, settings.Paths.Count);
		Assert.IsFalse(settings.Verbose);
	}

	[Test]
	public void SettingsFromArgs_ValidArgs_HasArgValues()
	{
		var args = new List<string>() { "-x", "cs", "." };
		var settings = _findOptions.SettingsFromArgs(args);
		var startInfo = new DirectoryInfo(".");
		Assert.AreEqual(1, settings.Paths.Count);
		Assert.True(settings.Paths.Contains("."));
		Assert.AreEqual(1, settings.InExtensions.Count);
		Assert.IsTrue(settings.InExtensions.Contains(".cs"));
	}

	[Test]
	public void SettingsFromArgs_InValidArgs_ThrowsFindException()
	{
		var args = new List<string>() { "-x", "cs", ".", "-Q" };
		var ex = Assert.Throws<FindException>(() => _findOptions.SettingsFromArgs(args));
		Assert.AreEqual("Invalid option: Q", ex!.Message);
	}

	[Test]
	public void SettingsFromJson_EqualsExpected()
	{
		var json = @"{
  ""path"": ""~/src/xfind/"",
  ""in-ext"": [""js"", ""ts""],
  ""out-dirpattern"": ""node_module"",
  ""out-filepattern"": [""temp""],
  ""debug"": true,
  ""includehidden"": false
}";
		var settings = new FindSettings();
		FindOptions.SettingsFromJson(json, settings);

		Assert.AreEqual(1, settings.Paths.Count);
		Assert.True(settings.Paths.Contains("~/src/xfind/"));

		Assert.AreEqual(2, settings.InExtensions.Count);
		Assert.True(settings.InExtensions.Contains(".js"));
		Assert.True(settings.InExtensions.Contains(".ts"));

		Assert.AreEqual(1, settings.OutDirPatterns.Count);
		Assert.AreEqual("node_module", settings.OutDirPatterns.First().ToString());

		Assert.AreEqual(1, settings.OutFilePatterns.Count);
		Assert.AreEqual("temp", settings.OutFilePatterns.First().ToString());

		Assert.True(settings.Debug);
		Assert.True(settings.ExcludeHidden);
	}
}
