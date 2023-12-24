namespace FsFindTests

open System
open System.IO
open NUnit.Framework
open FsFind

[<TestFixture>]
type FileUtilTests () =

    member this.FileTypes = FileTypes()

    [<SetUp>]
    member this.Setup () =
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
    // ExpandPath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.ExpandPath_WithTilde_ExpandHome () =
        let path = "~/src/git/xfind"
        let expected = FileUtil.JoinPath (FileUtil.GetHomePath()) (path.Substring(1))
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
    // NormalizePath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.NormalizePath_NoTrailingSlash_UnchangedPath () =
        let path = "~/src/git/xfind"
        Assert.That(FileUtil.NormalizePath(path), Is.EqualTo(path))
        ()

    [<Test>]
    member this.NormalizePath_TrailingSlash_TrimmedPath () =
        let path = "~/src/git/xfind/"
        Assert.That(FileUtil.NormalizePath(path), Is.EqualTo("~/src/git/xfind"))
        ()

    [<Test>]
    member this.NormalizePath_TrailingBackSlash_TrimmedPath () =
        let path = @"C:\src\git\xfind\"
        Assert.That(FileUtil.NormalizePath(path), Is.EqualTo(@"C:\src\git\xfind"))
        ()

    //////////////////////////////////////////////////////////////
    // JoinPath tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.JoinPath_NoTrailingSlash_EqualsExpected () =
        let path = "~/src/git/xfind/csharp/CsFind/CsFindTests"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + "/" + filename
        Assert.That(FileUtil.JoinPath path filename, Is.EqualTo(pathAndFile))
        ()

    [<Test>]
    member this.JoinPath_TrailingSlash_EqualsExpected () =
        let path = "~/src/git/xfind/csharp/CsFind/CsFindTests/"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + filename
        Assert.That(FileUtil.JoinPath path filename, Is.EqualTo(pathAndFile))
        ()

    [<Test>]
    member this.JoinPath_NoTrailingBackSlash_EqualsExpected () =
        let path = @"C:\src\git\xfind"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + "\\" + filename
        Assert.That(FileUtil.JoinPath path filename, Is.EqualTo(pathAndFile))
        ()

    [<Test>]
    member this.JoinPath_TrailingBackSlash_EqualsExpected () =
        let path = @"C:\src\git\xfind\"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + filename
        Assert.That(FileUtil.JoinPath path filename, Is.EqualTo(pathAndFile))
        ()

    [<Test>]
    member this.JoinPath_NoSlashes_EqualsExpected () =
        let path = "CsFindTests"
        let filename = "FileUtilTests.cs"
        let pathAndFile = path + "/" + filename
        Assert.That(FileUtil.JoinPath path filename, Is.EqualTo(pathAndFile))
        ()
