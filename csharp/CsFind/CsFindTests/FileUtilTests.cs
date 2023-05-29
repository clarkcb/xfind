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
		Assert.AreEqual("./rest/of/path", FileUtil.GetRelativePath(path, "."));
	}

	[Test]
	public void GetRelativePath_PathWithoutCurrentDirectory_FullPath()
	{
		const string path = "/a/full/path/by/itself";
		Assert.AreEqual(path, FileUtil.GetRelativePath(path, "/a/full/path"));
	}

	[Test]
	public void GetRelativePath_RelativePath_Unchanged()
	{
		const string path = "./a/relative/path";
		Assert.AreEqual(path, FileUtil.GetRelativePath(path, "."));
	}

	/*************************************************************
	 * IsDotDir tests
	*************************************************************/
	[Test]
	public void IsDotDir_IsSingleDot_IsDotDir()
	{
		const string dotDir = ".";
		Assert.IsTrue(FileUtil.IsDotDir(dotDir));
	}

	[Test]
	public void IsDotDir_IsSingleDotWithTrailingSlash_IsDotDir()
	{
		const string dotDir = "./";
		Assert.IsTrue(FileUtil.IsDotDir(dotDir));
	}

	[Test]
	public void IsDotDir_IsDoubleDot_IsDotDir()
	{
		const string dotDir = "..";
		Assert.IsTrue(FileUtil.IsDotDir(dotDir));
	}

	[Test]
	public void IsDotDir_IsDoubleDotWithTrailingSlash_IsDotDir()
	{
		const string dotDir = "../";
		Assert.IsTrue(FileUtil.IsDotDir(dotDir));
	}

	[Test]
	public void IsDotDir_IsNotDotDir_IsNotDotDir()
	{
		const string nonDotDir = "~/path";
		Assert.IsFalse(FileUtil.IsDotDir(nonDotDir));
	}

	/*************************************************************
	 * IsHidden tests
	*************************************************************/
	[Test]
	public void IsHidden_StartsWithDot_IsHidden()
	{
		var hiddenFile = new FileInfo(".FileUtilTests.cs");
		Assert.IsTrue(FileUtil.IsHiddenFile(hiddenFile));
	}

	[Test]
	public void IsHidden_NotStartsWithDot_NotIsHidden()
	{
		var csFindTestsPath = Environment.GetEnvironmentVariable("XFIND_PATH") + "/csharp/CsFind/CsFindTests";
		var hiddenFile = new FileInfo(csFindTestsPath + "/FileUtilTests.cs");
		Assert.IsFalse(FileUtil.IsHiddenFile(hiddenFile));
	}

	[Test]
	public void IsHidden_SingleDot_NotIsHidden()
	{
		var dotDir = new DirectoryInfo(".");
		Assert.IsFalse(FileUtil.IsHiddenFile(dotDir));
	}

	[Test]
	public void IsHidden_DoubleDot_NotIsHidden()
	{
		var dotDir = new DirectoryInfo("..");
		Assert.IsFalse(FileUtil.IsHiddenFile(dotDir));
	}

	/*************************************************************
	 * ExpandPath tests
	*************************************************************/
	[Test]
	public void ExpandPath_WithTilde_ExpandHome()
	{
		const string path = "~/src/git/xfind";
		var expected = FileUtil.JoinPath(FileUtil.GetHomePath(), path.Substring(1));
		var actual = FileUtil.ExpandPath(path);
		Assert.AreEqual(expected, actual);
	}

	[Test]
	public void ExpandPath_NoTilde_UnchangedPath()
	{
		var path = "/a/full/path/";
		Assert.AreEqual(path, FileUtil.ExpandPath(path));
	}

	[Test]
	public void ExpandPath_WithBackSlashes_UnchangedPath()
	{
		const string path = @"C:\src\git\xfind\";
		Assert.AreEqual(path, FileUtil.ExpandPath(path));
	}

	/*************************************************************
	 * NormalizePath tests
	*************************************************************/
	[Test]
	public void NormalizePath_NoTrailingSlash_UnchangedPath()
	{
		const string path = "~/src/git/xfind";
		Assert.AreEqual(path, FileUtil.NormalizePath(path));
	}

	[Test]
	public void NormalizePath_TrailingSlash_TrimmedPath()
	{
		const string path = "~/src/git/xfind/";
		Assert.AreEqual("~/src/git/xfind", FileUtil.NormalizePath(path));
	}

	[Test]
	public void NormalizePath_TrailingBackSlash_TrimmedPath()
	{
		const string path = @"C:\src\git\xfind\";
		Assert.AreEqual(@"C:\src\git\xfind", FileUtil.NormalizePath(path));
	}

	/*************************************************************
	 * JoinPath tests
	*************************************************************/
	[Test]
	public void JoinPath_NoTrailingSlash_EqualsExpected()
	{
		const string path = "~/src/git/xfind/csharp/CsFind/CsFindTests";
		const string filename = "FileUtilTests.cs";
		var pathAndFile = path + "/" + filename;
		Assert.AreEqual(pathAndFile, FileUtil.JoinPath(path, filename));
	}

	[Test]
	public void JoinPath_TrailingSlash_EqualsExpected()
	{
		const string path = "~/src/git/xfind/csharp/CsFind/CsFindTests/";
		const string filename = "FileUtilTests.cs";
		var pathAndFile = path + filename;
		Assert.AreEqual(pathAndFile, FileUtil.JoinPath(path, filename));
	}

	[Test]
	public void JoinPath_NoTrailingBackSlash_EqualsExpected()
	{
		const string path = @"C:\src\git\xfind";
		const string filename = "FileUtilTests.cs";
		var pathAndFile = path + "\\" + filename;
		Assert.AreEqual(pathAndFile, FileUtil.JoinPath(path, filename));
	}

	[Test]
	public void JoinPath_TrailingBackSlash_EqualsExpected()
	{
		const string path = @"C:\src\git\xfind\";
		const string filename = "FileUtilTests.cs";
		var pathAndFile = path + filename;
		Assert.AreEqual(pathAndFile, FileUtil.JoinPath(path, filename));
	}

	[Test]
	public void JoinPath_NoSlashes_EqualsExpected()
	{
		const string path = "CsFindTests";
		const string filename = "FileUtilTests.cs";
		var pathAndFile = path + "/" + filename;
		Assert.AreEqual(pathAndFile, FileUtil.JoinPath(path, filename));
	}
}
