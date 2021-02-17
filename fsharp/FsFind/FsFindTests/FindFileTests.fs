namespace FsFindTests

open System.IO
open NUnit.Framework
open FsFind

[<TestFixture>]
type FindFileTests () =

    member this.CsFindPath = "~/src/xfind/csharp/CsFind/CsFind"
    member this.WinCsFindPath = @"C:\src\xfind\csharp\CsFind\CsFind"

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.FindFile_ToString_EqualsExpected () =
        let findFile = FindFile.Create (FileInfo(FileUtil.JoinPath this.CsFindPath "Finder.cs")) FileType.Code
        Assert.AreEqual(this.CsFindPath + "/Finder.cs", FindFile.ToString(findFile))
        ()

    [<Test>]
    member this.FindFileTrailingSlash_ToString_EqualsExpected () =
        let findFile = FindFile.Create (FileInfo(FileUtil.JoinPath this.CsFindPath "Finder.cs")) FileType.Code
        Assert.AreEqual(this.CsFindPath + "/Finder.cs", FindFile.ToString(findFile))
        ()

    [<Test>]
    member this.FindFileBackSlashes_ToString_EqualsExpected () =
        let findFile = FindFile.Create (FileInfo(FileUtil.JoinPath this.WinCsFindPath "Finder.cs")) FileType.Code
        Assert.AreEqual(this.WinCsFindPath + @"\Finder.cs", FindFile.ToString(findFile))
        ()

    [<Test>]
    member this.FindFileBackSlashesTrailingSlash_ToString_EqualsExpected () =
        let findFile = FindFile.Create (FileInfo(FileUtil.JoinPath this.WinCsFindPath "Finder.cs")) FileType.Code
        Assert.AreEqual(this.WinCsFindPath + @"\Finder.cs", FindFile.ToString(findFile))
        ()
