using NUnit.Framework;
using CsFindLib;

namespace CsFindTests;

[TestFixture]
class FileResultTests
{
	private const string CsFinderPath = "~/src/xfind/csharp/CsFind/CsFind/Finder.cs";
	// This is temporary until all versions support preserving tilde in path
	private readonly string _expandedCsFinderPath = $"{FileUtil.GetHomePath()}/src/xfind/csharp/CsFind/CsFind/Finder.cs";

	[Test]
	public void FileResult_ToString_EqualsExpected()
	{
		var fileResult = new FileResult(CsFinderPath, FileType.Text);
		// Assert.That(fileResult.ToString(), Is.EqualTo(CsFinderPath));
		Assert.That(fileResult.ToString(), Is.EqualTo(_expandedCsFinderPath));
	}

	[Test]
	public void FileResultTrailingSlash_ToString_EqualsExpected()
	{
		var fileResult = new FileResult(CsFinderPath, FileType.Code);
		// Assert.That(fileResult.ToString(), Is.EqualTo(CsFinderPath));
		Assert.That(fileResult.ToString(), Is.EqualTo(_expandedCsFinderPath));
	}
}
