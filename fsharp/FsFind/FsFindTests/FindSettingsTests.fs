namespace FsFindTests

open NUnit.Framework
open FsFind

[<TestFixture>]
type FindSettingTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.GetNewFindSettings_NoModifications_HasDefaultValues () =
        let settings = FindSettings()
        Assert.IsFalse(settings.ArchivesOnly)
        Assert.IsFalse(settings.Debug)
        Assert.IsFalse(settings.IncludeArchives)
        Assert.IsFalse(settings.IncludeHidden)
        Assert.IsFalse(settings.ListDirs)
        Assert.IsFalse(settings.ListFiles)
        Assert.AreEqual(None, settings.MaxLastMod)
        Assert.AreEqual(0, settings.MaxSize)
        Assert.AreEqual(None, settings.MinLastMod)
        Assert.AreEqual(0, settings.MinSize)
        Assert.IsEmpty(settings.Paths)
        Assert.IsFalse(settings.PrintUsage)
        Assert.IsFalse(settings.PrintVersion)
        Assert.IsTrue(settings.Recursive)
        Assert.AreEqual(SortBy.FilePath, settings.SortBy)
        Assert.IsFalse(settings.SortCaseInsensitive)
        Assert.IsFalse(settings.SortDescending)
        Assert.IsFalse(settings.Verbose)
        ()

    [<Test>]
    member this.FindSettings_AddExtensions_HasExtensions () =
        let settings = FindSettings()
        settings.InExtensions <- settings.AddExtensions "cs" settings.InExtensions
        Assert.AreEqual(1, settings.InExtensions.Length)
        Assert.AreEqual(".cs", settings.InExtensions.Head)
        settings.InExtensions <- settings.AddExtensions "java,scala" settings.InExtensions
        Assert.AreEqual(3, settings.InExtensions.Length)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".java"))
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".scala"))
        ()

    [<Test>]
    member this.FindSettings_AddPatterns_HasPatterns () =
        let settings = FindSettings()
        settings.InFilePatterns <- settings.AddPattern "Find" settings.InFilePatterns
        Assert.AreEqual(1, settings.InFilePatterns.Length)
        Assert.AreEqual("Find", settings.InFilePatterns.Head.ToString())
        ()

    [<Test>]
    member this.FindSettings_SetArchivesOnly_HasIncludeArchives () =
        let settings = FindSettings()
        settings.ArchivesOnly <- true
        Assert.IsTrue(settings.ArchivesOnly)
        Assert.IsTrue(settings.IncludeArchives)
        ()

    [<Test>]
    member this.FindSettings_SetDebug_HasVerbose () =
        let settings = FindSettings()
        settings.Debug <- true
        Assert.IsTrue(settings.Debug)
        Assert.IsTrue(settings.Verbose)
        ()
