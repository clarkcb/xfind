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
        Assert.That(settings.ArchivesOnly, Is.False)
        Assert.That(settings.Debug, Is.False)
        Assert.That(settings.IncludeArchives, Is.False)
        Assert.That(settings.IncludeHidden, Is.False)
        Assert.That(settings.MaxLastMod, Is.EqualTo(None))
        Assert.That(settings.MaxSize, Is.EqualTo(0))
        Assert.That(settings.MinLastMod, Is.EqualTo(None))
        Assert.That(settings.MinSize, Is.EqualTo(0))
        Assert.That(settings.Paths, Is.Empty)
        Assert.That(settings.PrintDirs, Is.False)
        Assert.That(settings.PrintFiles, Is.False)
        Assert.That(settings.PrintUsage, Is.False)
        Assert.That(settings.PrintVersion, Is.False)
        Assert.That(settings.Recursive)
        Assert.That(settings.SortBy, Is.EqualTo(SortBy.FilePath))
        Assert.That(settings.SortCaseInsensitive, Is.False)
        Assert.That(settings.SortDescending, Is.False)
        Assert.That(settings.Verbose, Is.False)
        ()

    [<Test>]
    member this.FindSettings_AddExtensions_HasExtensions () =
        let settings = FindSettings()
        settings.InExtensions <- settings.AddExtensions "cs" settings.InExtensions
        Assert.That(settings.InExtensions.Length, Is.EqualTo(1))
        Assert.That(settings.InExtensions.Head, Is.EqualTo(".cs"))
        settings.InExtensions <- settings.AddExtensions "java,scala" settings.InExtensions
        Assert.That(settings.InExtensions.Length, Is.EqualTo(3))
        Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".java"))
        Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".scala"))
        ()

    [<Test>]
    member this.FindSettings_AddPatterns_HasPatterns () =
        let settings = FindSettings()
        settings.InFilePatterns <- settings.AddPattern "Find" settings.InFilePatterns
        Assert.That(settings.InFilePatterns.Length, Is.EqualTo(1))
        Assert.That(settings.InFilePatterns.Head.ToString(), Is.EqualTo("Find"))
        ()

    [<Test>]
    member this.FindSettings_SetArchivesOnly_HasIncludeArchives () =
        let settings = FindSettings()
        settings.ArchivesOnly <- true
        Assert.That(settings.ArchivesOnly)
        Assert.That(settings.IncludeArchives)
        ()

    [<Test>]
    member this.FindSettings_SetDebug_HasVerbose () =
        let settings = FindSettings()
        settings.Debug <- true
        Assert.That(settings.Debug)
        Assert.That(settings.Verbose)
        ()
