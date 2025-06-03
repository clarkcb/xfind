namespace FsFindTests

open System
open System.IO
open NUnit.Framework
open FsFindLib

[<TestFixture>]
type FileUtilTests () =

    member this.FileTypes = FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    //////////////////////////////////////////////////////////////
    // ExpandPath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.ExpandPath_Tilde_ExpandHome () =
        let path = "~"
        let expected = Path.Join(FileUtil.GetHomePath())
        Assert.That(FileUtil.ExpandPath(path), Is.EqualTo(expected))
        ()

    [<Test>]
    member this.ExpandPath_WithTilde_ExpandHome () =
        let path = "~/src/xfind"
        let expected = Path.Join(FileUtil.GetHomePath(), path.Substring(2))
        let actual = FileUtil.ExpandPath(path)
        Assert.That(actual, Is.EqualTo(expected))
        ()

    [<Test>]
    member this.ExpandPath_WithTildeAndName_ExpandHome () =
        let path = "~cary/src/xfind"
        let expected = Path.Join(Path.GetDirectoryName(FileUtil.GetHomePath()), path.Substring(1))
        let actual = FileUtil.ExpandPath(path)
        Assert.That(actual, Is.EqualTo(expected))
        ()

    [<Test>]
    member this.ExpandPath_NoTilde_UnchangedPath () =
        let path = "/a/full/path/"
        Assert.That(FileUtil.ExpandPath(path), Is.EqualTo(path))
        ()

    [<Test>]
    member this.ExpandPath_WithBackSlashes_UnchangedPath () =
        let path = @"C:\src\git\xfind\"
        Assert.That(FileUtil.ExpandPath(path), Is.EqualTo(path))
        ()


    //////////////////////////////////////////////////////////////
    // GetRelativePath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.GetRelativePath_PathWithCurrentDirectory_RelativePath () =
        let path = Environment.CurrentDirectory + "/rest/of/path/"
        Assert.That(FileUtil.GetRelativePath path ".", Is.EqualTo("./rest/of/path/"))
        ()

    [<Test>]
    member this.GetRelativePath_PathWithoutCurrentDirectory_FullPath () =
        let path = "/a/full/path/by/itself/"
        Assert.That(FileUtil.GetRelativePath path "/a/full/path", Is.EqualTo(path))
        ()

    [<Test>]
    member this.GetRelativePath_RelativePath_Unchanged () =
        let path = "./a/relative/path/"
        Assert.That(FileUtil.GetRelativePath path ".", Is.EqualTo(path))
        ()

    //////////////////////////////////////////////////////////////
    // IsDotDir tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.IsDotDir_IsSingleDot_IsDotDir () =
        let dotDir = "."
        Assert.That(FileUtil.IsDotDir(dotDir))
        ()

    [<Test>]
    member this.IsDotDir_IsSingleDotWithTrailingSlash_IsDotDir () =
        let dotDir = "./"
        Assert.That(FileUtil.IsDotDir(dotDir))
        ()

    [<Test>]
    member this.IsDotDir_IsDoubleDot_IsDotDir () =
        let dotDir = ".."
        Assert.That(FileUtil.IsDotDir(dotDir))
        ()

    [<Test>]
    member this.IsDotDir_IsDoubleDotWithTrailingSlash_IsDotDir () =
        let dotDir = "../"
        Assert.That(FileUtil.IsDotDir(dotDir))
        ()

    [<Test>]
    member this.IsDotDir_IsNotDotDir_IsNotDotDir () =
        let nonDotDir = "~/path"
        Assert.That(FileUtil.IsDotDir(nonDotDir), Is.False)
        ()

    //////////////////////////////////////////////////////////////
    // IsHidden tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.IsHidden_StartsWithDot_IsHidden () =
        let hiddenFile = FileInfo(".FileUtilTests.cs")
        Assert.That(FileUtil.IsHiddenFile(hiddenFile))
        ()

    [<Test>]
    member this.IsHidden_NotStartsWithDot_NotIsHidden () =
        let hiddenFile = FileInfo("FileUtilTests.cs")
        Assert.That(FileUtil.IsHiddenFile(hiddenFile), Is.False)
        ()

    [<Test>]
    member this.IsHidden_SingleDot_NotIsHidden () =
        let dotDir = DirectoryInfo(".")
        Assert.That(FileUtil.IsHiddenFile(dotDir), Is.False)
        ()

    [<Test>]
    member this.IsHidden_DoubleDot_NotIsHidden () =
        let dotDir = DirectoryInfo("..")
        Assert.That(FileUtil.IsHiddenFile(dotDir), Is.False)
        ()

    //////////////////////////////////////////////////////////////
    // NormalizePath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.NormalizePath_NoTrailingSlash_UnchangedPath () =
        let path = "~/src/xfind"
        Assert.That(FileUtil.NormalizePath(path), Is.EqualTo(path))
        ()

    [<Test>]
    member this.NormalizePath_TrailingSlash_TrimmedPath () =
        let path = "~/src/xfind/"
        Assert.That(FileUtil.NormalizePath(path), Is.EqualTo("~/src/xfind"))
        ()

    //////////////////////////////////////////////////////////////
    // JoinPath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.JoinPath_NoTrailingSlash_EqualsExpected () =
        let path = "~/src/xfind/csharp/CsFind/CsFindTests"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + "/" + filename
        Assert.That(Path.Join(path, filename), Is.EqualTo(pathAndFile))
        ()

    [<Test>]
    member this.JoinPath_TrailingSlash_EqualsExpected () =
        let path = "~/src/xfind/csharp/CsFind/CsFindTests/"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + filename
        Assert.That(Path.Join(path, filename), Is.EqualTo(pathAndFile))
        ()

    [<Test>]
    member this.JoinPath_NoSlashes_EqualsExpected () =
        let path = "CsFindTests"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + "/" + filename
        Assert.That(Path.Join(path, filename), Is.EqualTo(pathAndFile))
        ()
