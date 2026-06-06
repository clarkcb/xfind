namespace FsFindTests

open System.IO
open NUnit.Framework
open FsFindLib

[<TestFixture>]
type FileResultTests () =

    member this.CsFindPath = "~/src/xfind/csharp/CsFind/CsFind"
    member this.WinCsFindPath = @"C:\src\xfind\csharp\CsFind\CsFind"

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.FileResult_ToString_EqualsExpected () =
        let fi = Path.Join(this.CsFindPath, "Finder.cs")
        let fileType = FileType.Code
        let fr = FileResult.Create fi fileType 0L None
        Assert.That(FileResult.ToString(fr), Is.EqualTo($"%s{this.CsFindPath}/Finder.cs"))
        ()

    [<Test>]
    member this.FileResultTrailingSlash_ToString_EqualsExpected () =
        let fi = Path.Join($"%s{this.CsFindPath}/", "Finder.cs")
        let fileType = FileType.Code
        let fr = FileResult.Create fi fileType 0L None
        Assert.That(FileResult.ToString(fr), Is.EqualTo($"%s{this.CsFindPath}/Finder.cs"))
        ()
