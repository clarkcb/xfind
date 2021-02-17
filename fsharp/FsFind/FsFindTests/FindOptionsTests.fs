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
        Assert.IsTrue(settings.Colorize)
        Assert.IsFalse(settings.Debug)
        Assert.IsTrue(settings.ExcludeHidden)
        Assert.IsFalse(settings.FirstMatch)
        Assert.AreEqual(0, settings.LinesAfter)
        Assert.AreEqual(0, settings.LinesBefore)
        Assert.IsFalse(settings.ListDirs)
        Assert.IsFalse(settings.ListFiles)
        Assert.IsFalse(settings.ListLines)
        Assert.AreEqual(150, settings.MaxLineLength)
        Assert.IsFalse(settings.MultiLineFind)
        Assert.IsTrue(settings.PrintResults)
        Assert.IsFalse(settings.PrintUsage)
        Assert.IsFalse(settings.PrintVersion)
        Assert.IsTrue(settings.Recursive)
        Assert.IsFalse(settings.FindArchives)
        Assert.AreEqual("", settings.StartPath)
        Assert.AreEqual("utf-8", settings.TextFileEncoding)
        Assert.IsFalse(settings.UniqueLines)
        Assert.IsFalse(settings.Verbose)
        ()

    [<Test>]
    member this.SettingsFromArgs_ValidArgs_HasArgValues () =
        let args = [| "-x"; "cs"; "-s"; "Find"; "." |]
        let settings, _ = FindOptions.SettingsFromArgs(args)
        //let startFile = FileInfo(".")
        //Assert.AreEqual(settings.StartPath, startFile.FullName)
        Assert.AreEqual(1, settings.InExtensions.Length)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".cs"))
        Assert.AreEqual(1, settings.FindPatterns.Length)
        Assert.AreEqual("Find", settings.FindPatterns.Head.ToString())
        ()

    [<Test>]
    member this.SettingsFromArgs_InValidArgs_ThrowsFindException () =
        let args = [| "-x"; "cs"; "-s"; "Find"; "."; "-Q" |]
        let _, err = FindOptions.SettingsFromArgs(args)
        Assert.AreEqual("Invalid option: Q", err)
        ()
