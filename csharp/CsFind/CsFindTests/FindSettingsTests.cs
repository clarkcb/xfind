using System.Linq;
using CsFindLib;
using NUnit.Framework;

namespace CsFindTests;

[TestFixture]
public class FindSettingsTests
{
	[Test]
	public void GetNewFindSettings_NoModifications_HasDefaultValues()
	{
		var settings = new FindSettings();
		Assert.That(settings.ArchivesOnly, Is.False);
		Assert.That(settings.Debug, Is.False);
		Assert.That(settings.FollowSymlinks, Is.False);
		Assert.That(settings.IncludeArchives, Is.False);
		Assert.That(settings.IncludeHidden, Is.False);
		Assert.That(settings.PrintDirs, Is.False);
		Assert.That(settings.PrintFiles, Is.False);
		Assert.That(settings.PrintUsage, Is.False);
		Assert.That(settings.PrintVersion, Is.False);
		Assert.That(settings.Recursive);
		Assert.That(settings.Verbose, Is.False);
	}

	[Test]
	public void FindSettings_AddExtensions_HasExtensions()
	{
		var settings = new FindSettings();
		settings.AddInExtension("cs");
		Assert.That(settings.InExtensions.Count, Is.EqualTo(1));
		Assert.That(settings.InExtensions.Contains(".cs"));
		settings.AddInExtension("java,scala");
		Assert.That(settings.InExtensions.Count, Is.EqualTo(3));
		Assert.That(settings.InExtensions.Contains(".java"));
		Assert.That(settings.InExtensions.Contains(".scala"));
	}

	[Test]
	public void FindSettings_AddPatterns_HasPatterns()
	{
		var settings = new FindSettings();
		settings.AddInFilePattern("Find");
		Assert.That(1, Is.EqualTo(settings.InFilePatterns.Count));
		Assert.That(settings.InFilePatterns.First().ToString() == "Find");
	}

	[Test]
	public void FindSettings_SetArchivesOnly_HasIncludeArchives()
	{
		var settings = new FindSettings {ArchivesOnly = true};
		Assert.That(settings.ArchivesOnly);
		Assert.That(settings.IncludeArchives);
	}

	[Test]
	public void FindSettings_SetDebug_HasVerbose()
	{
		var settings = new FindSettings {Debug = true};
		Assert.That(settings.Debug);
		Assert.That(settings.Verbose);
	}
}
