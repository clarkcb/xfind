using System.Collections.Generic;
using System.Linq;
using CsFindLib;
using NUnit.Framework;
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
		Assert.That(settings.ArchivesOnly, Is.False);
		Assert.That(settings.Debug, Is.False);
		Assert.That(settings.IncludeArchives, Is.False);
		Assert.That(settings.IncludeHidden, Is.False);
		Assert.That(settings.PrintDirs, Is.False);
		Assert.That(settings.PrintFiles);
		Assert.That(settings.PrintUsage, Is.False);
		Assert.That(settings.PrintVersion, Is.False);
		Assert.That(settings.Recursive);
		Assert.That(settings.Paths.Count, Is.EqualTo(0));
		Assert.That(settings.Verbose, Is.False);
	}

	[Test]
	public void SettingsFromArgs_ValidArgs_HasArgValues()
	{
		var args = new List<string> { "-x", "cs", "." };
		var settings = _findOptions.SettingsFromArgs(args);
		Assert.That(settings.Paths.Count, Is.EqualTo(1));
		Assert.That(settings.Paths.Contains("."));
		Assert.That(settings.InExtensions.Count, Is.EqualTo(1));
		Assert.That(settings.InExtensions.Contains(".cs"));
	}

	[Test]
	public void SettingsFromArgs_InValidArgs_ThrowsFindException()
	{
		var args = new List<string> { "-x", "cs", ".", "-Q" };
		var ex = Assert.Throws<FindException>(() => _findOptions.SettingsFromArgs(args));
		Assert.That(ex!.Message, Is.EqualTo("Invalid option: Q"));
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
  ""includehidden"": true
}";
		var settings = new FindSettings();
		FindOptions.SettingsFromJson(json, settings);

		Assert.That(settings.Paths.Count, Is.EqualTo(1));
		Assert.That(settings.Paths.Contains("~/src/xfind/"));

		Assert.That(settings.InExtensions.Count, Is.EqualTo(2));
		Assert.That(settings.InExtensions.Contains(".js"));
		Assert.That(settings.InExtensions.Contains(".ts"));

		Assert.That(settings.OutDirPatterns.Count, Is.EqualTo(1));
		Assert.That(settings.OutDirPatterns.First().ToString(), Is.EqualTo("node_module"));

		Assert.That(settings.OutFilePatterns.Count, Is.EqualTo(1));
		Assert.That(settings.OutFilePatterns.First().ToString(), Is.EqualTo("temp"));

		Assert.That(settings.Debug);
		Assert.That(settings.IncludeHidden);
	}
}
