namespace FsFindTests

open System.IO
open NUnit.Framework
open FsFind

[<TestFixture>]
type FileResultTests () =

    member this.CsFindPath = "~/src/xfind/csharp/CsFind/CsFind"
    member this.WinCsFindPath = @"C:\src\xfind\csharp\CsFind\CsFind"

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.FileResult_ToString_EqualsExpected () =
        let fi = FileInfo(FileUtil.JoinPath this.CsFindPath "Finder.cs")
        let fileType = FileType.Code
        let fr = FileResult.Create fi fileType
        Assert.That(FileResult.ToString(fr), Is.EqualTo($"%s{this.CsFindPath}/Finder.cs"))
        ()

    [<Test>]
    member this.FileResultTrailingSlash_ToString_EqualsExpected () =
        let fi = FileInfo(FileUtil.JoinPath $"%s{this.CsFindPath}/" "Finder.cs")
        let fileType = FileType.Code
        let fr = FileResult.Create fi fileType
        Assert.That(FileResult.ToString(fr), Is.EqualTo($"%s{this.CsFindPath}/Finder.cs"))
        ()

    [<Test>]
    member this.FileResultBackSlashes_ToString_EqualsExpected () =
        let fi = FileInfo(FileUtil.JoinPath this.WinCsFindPath "Finder.cs")
        let fileType = FileType.Code
        let fr = FileResult.Create fi fileType
        Assert.That(FileResult.ToString(fr), Is.EqualTo($"%s{this.WinCsFindPath}\\Finder.cs"))
        ()

    [<Test>]
    member this.FileResultBackSlashesTrailingSlash_ToString_EqualsExpected () =
        let fi = FileInfo(FileUtil.JoinPath $"%s{this.WinCsFindPath}\\" "Finder.cs")
        let fileType = FileType.Code
        let fr = FileResult.Create fi fileType
        Assert.That(FileResult.ToString(fr), Is.EqualTo($"%s{this.WinCsFindPath}\\Finder.cs"))
        ()
