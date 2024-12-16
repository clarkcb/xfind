using System;
using System.IO;
using NUnit.Framework;
using CsFindLib;

namespace CsFindTests;

[TestFixture]
class FileUtilTests
{
	/*************************************************************
	 * GetRelativePath tests
	*************************************************************/
	[Test]
	public void GetRelativePath_PathWithCurrentDirectory_RelativePath()
	{
		var path = Environment.CurrentDirectory + "/rest/of/path/";
		Assert.That(FileUtil.GetRelativePath(path, "."), Is.EqualTo("./rest/of/path"));
	}

	[Test]
	public void GetRelativePath_PathWithoutCurrentDirectory_FullPath()
	{
		const string path = "/a/full/path/by/itself";
		Assert.That(FileUtil.GetRelativePath(path, "/a/full/path"), Is.EqualTo(path));
	}

	[Test]
	public void GetRelativePath_RelativePath_Unchanged()
	{
		const string path = "./a/relative/path";
		Assert.That(FileUtil.GetRelativePath(path, "."), Is.EqualTo(path));
	}

	/*************************************************************
	 * IsDotDir tests
	*************************************************************/
	[Test]
	public void IsDotDir_IsSingleDot_IsDotDir()
	{
		const string dotDir = ".";
		Assert.That(FileUtil.IsDotDir(dotDir));
	}

	[Test]
	public void IsDotDir_IsSingleDotWithTrailingSlash_IsDotDir()
	{
		const string dotDir = "./";
		Assert.That(FileUtil.IsDotDir(dotDir));
	}

	[Test]
	public void IsDotDir_IsDoubleDot_IsDotDir()
	{
		const string dotDir = "..";
		Assert.That(FileUtil.IsDotDir(dotDir));
	}

	[Test]
	public void IsDotDir_IsDoubleDotWithTrailingSlash_IsDotDir()
	{
		const string dotDir = "../";
		Assert.That(FileUtil.IsDotDir(dotDir));
	}

	[Test]
	public void IsDotDir_IsNotDotDir_IsNotDotDir()
	{
		const string nonDotDir = "~/path";
		Assert.That(FileUtil.IsDotDir(nonDotDir), Is.False);
	}

	/*************************************************************
	 * IsHidden tests
	*************************************************************/
	[Test]
	public void IsHidden_StartsWithDot_IsHidden()
	{
		var hiddenFile = new FileInfo(".FileUtilTests.cs");
		Assert.That(FileUtil.IsHiddenFile(hiddenFile));
	}

	[Test]
	public void IsHidden_NotStartsWithDot_NotIsHidden()
	{
		var csFindTestsPath = Environment.GetEnvironmentVariable("XFIND_PATH") + "/csharp/CsFind/CsFindTests";
		var hiddenFile = new FileInfo(csFindTestsPath + "/FileUtilTests.cs");
		Assert.That(FileUtil.IsHiddenFile(hiddenFile), Is.False);
	}

	[Test]
	public void IsHidden_SingleDot_NotIsHidden()
	{
		var dotDir = new DirectoryInfo(".");
		Assert.That(FileUtil.IsHiddenFile(dotDir), Is.False);
	}

	[Test]
	public void IsHidden_DoubleDot_NotIsHidden()
	{
		var dotDir = new DirectoryInfo("..");
		Assert.That(FileUtil.IsHiddenFile(dotDir), Is.False);
	}

	/*************************************************************
	 * ExpandPath tests
	*************************************************************/
	[Test]
	public void ExpandPath_Tilde_ExpandHome()
	{
		const string path = "~";
		var expected = FileUtil.GetHomePath();
		var actual = FileUtil.ExpandPath(path);
		Assert.That(actual, Is.EqualTo(expected));
	}

	[Test]
	public void ExpandPath_WithTilde_ExpandHome()
	{
		const string path = "~/src/xfind";
		var expected = Path.Join(FileUtil.GetHomePath(), path.Substring(1));
		var actual = FileUtil.ExpandPath(path);
		Assert.That(actual, Is.EqualTo(expected));
	}

	[Test]
	public void ExpandPath_WithTildeAndName_ExpandHome()
	{
		const string path = "~cary/src/xfind";
		var homePath = Path.GetDirectoryName(FileUtil.GetHomePath());
		var expected = Path.Join(homePath, path.Substring(1));
		var actual = FileUtil.ExpandPath(path);
		Assert.That(actual, Is.EqualTo(expected));
	}

	[Test]
	public void ExpandPath_NoTilde_UnchangedPath()
	{
		var path = "/a/full/path/";
		Assert.That(FileUtil.ExpandPath(path), Is.EqualTo(path));
	}

	/*************************************************************
	 * NormalizePath tests
	*************************************************************/
	[Test]
	public void NormalizePath_NoTrailingSlash_UnchangedPath()
	{
		const string path = "~/src/xfind";
		Assert.That(FileUtil.NormalizePath(path), Is.EqualTo(path));
	}

	[Test]
	public void NormalizePath_TrailingSlash_TrimmedPath()
	{
		const string path = "~/src/xfind/";
		Assert.That(FileUtil.NormalizePath(path), Is.EqualTo("~/src/xfind"));
	}

	/*************************************************************
	 * JoinPath tests
	*************************************************************/
	[Test]
	public void JoinPath_NoTrailingSlash_EqualsExpected()
	{
		const string path = "~/src/xfind/csharp/CsFind/CsFindTests";
		const string filename = "FileUtilTests.cs";
		var pathAndFile = path + "/" + filename;
		Assert.That(Path.Join(path, filename), Is.EqualTo(pathAndFile));
	}

	[Test]
	public void JoinPath_TrailingSlash_EqualsExpected()
	{
		const string path = "~/src/xfind/csharp/CsFind/CsFindTests/";
		const string filename = "FileUtilTests.cs";
		var pathAndFile = path + filename;
		Assert.That(Path.Join(path, filename), Is.EqualTo(pathAndFile));
	}

	[Test]
	public void JoinPath_NoSlashes_EqualsExpected()
	{
		const string path = "CsFindTests";
		const string filename = "FileUtilTests.cs";
		var pathAndFile = path + "/" + filename;
		Assert.That(Path.Join(path, filename), Is.EqualTo(pathAndFile));
	}
}
