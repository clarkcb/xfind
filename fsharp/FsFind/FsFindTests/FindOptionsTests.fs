namespace FsFindTests

open NUnit.Framework
open FsFind

[<TestFixture>]
type FindOptionsTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.SettingsFromArgs_NoArgs_HasDefaultValues () =
        let args : string[] = [||]
        let settings, _ = FindOptions.SettingsFromArgs(args)
        Assert.IsFalse(settings.ArchivesOnly)
        Assert.IsFalse(settings.Debug)
        Assert.IsFalse(settings.IncludeArchives)
        Assert.IsFalse(settings.IncludeHidden)
        Assert.IsFalse(settings.ListDirs)
        Assert.IsTrue(settings.ListFiles)
        Assert.AreEqual(0, settings.Paths.Length)
        Assert.IsFalse(settings.PrintUsage)
        Assert.IsFalse(settings.PrintVersion)
        Assert.IsTrue(settings.Recursive)
        Assert.AreEqual(SortBy.FilePath, settings.SortBy)
        Assert.IsFalse(settings.SortCaseInsensitive)
        Assert.IsFalse(settings.SortDescending)
        Assert.IsFalse(settings.Verbose)
        ()

    [<Test>]
    member this.SettingsFromArgs_ValidArgs_HasArgValues () =
        let args = [| "-x"; "cs"; "." |]
        let settings, _ = FindOptions.SettingsFromArgs(args)
        Assert.AreEqual(1, settings.InExtensions.Length)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".cs"))
        Assert.AreEqual(1, settings.Paths.Length)
        Assert.AreEqual(".", settings.Paths.Head.ToString())
        ()

    [<Test>]
    member this.SettingsFromArgs_InValidArgs_ThrowsFindException () =
        let args = [| "-x"; "cs"; "."; "-Q" |]
        let _, err = FindOptions.SettingsFromArgs(args)
        Assert.AreEqual("Invalid option: Q", err)
        ()
