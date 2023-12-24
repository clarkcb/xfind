using NUnit.Framework;
using CsFindLib;

namespace CsFindTests;

[TestFixture]
class FileResultTests
{
	private const string CsFindPath = "~/src/xfind/csharp/CsFind/CsFind";
	private const string WinCsFindPath = @"C:\src\git\xfind\csharp\CsFind\CsFind";

	[Test]
	public void FileResult_ToString_EqualsExpected()
	{
		var fileResult = new FileResult(CsFindPath, "Finder.cs", FileType.Text);
		Assert.That(fileResult.ToString(), Is.EqualTo(CsFindPath + "/Finder.cs"));
	}

	[Test]
	public void FileResultTrailingSlash_ToString_EqualsExpected()
	{
		var fileResult = new FileResult(CsFindPath + "/", "Finder.cs", FileType.Text);
		Assert.That(fileResult.ToString(), Is.EqualTo(CsFindPath + "/Finder.cs"));
	}

	[Test]
	public void FileResultBackSlashes_ToString_EqualsExpected()
	{
		var fileResult = new FileResult(WinCsFindPath, "Finder.cs", FileType.Text);
		Assert.That(fileResult.ToString(), Is.EqualTo(WinCsFindPath + @"\Finder.cs"));
	}

	[Test]
	public void FileResultBackSlashesTrailingSlash_ToString_EqualsExpected()
	{
		var fileResult = new FileResult(WinCsFindPath + @"\", "Finder.cs", FileType.Text);
		Assert.That(fileResult.ToString(), Is.EqualTo(WinCsFindPath + @"\Finder.cs"));
	}
}
