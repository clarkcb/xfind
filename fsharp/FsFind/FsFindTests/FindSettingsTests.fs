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
        let settings = FindSettings.DefaultSettings
        Assert.IsFalse(settings.ArchivesOnly)
        Assert.IsFalse(settings.Debug)
        Assert.IsTrue(settings.ExcludeHidden)
        Assert.IsFalse(settings.ListDirs)
        Assert.IsFalse(settings.ListFiles)
        Assert.IsFalse(settings.PrintUsage)
        Assert.IsFalse(settings.PrintVersion)
        Assert.IsTrue(settings.Recursive)
        Assert.IsFalse(settings.IncludeArchives)
        Assert.IsFalse(settings.Verbose)
        ()

    [<Test>]
    member this.FindSettings_AddExtensions_HasExtensions () =
        let settings = FindSettings.DefaultSettings
        let settings = { settings with InExtensions = FindSettings.AddExtensions "cs" settings.InExtensions }
        Assert.AreEqual(1, settings.InExtensions.Length)
        Assert.AreEqual(".cs", settings.InExtensions.Head)
        let settings = { settings with InExtensions = FindSettings.AddExtensions "java,scala" settings.InExtensions }
        Assert.AreEqual(3, settings.InExtensions.Length)
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".java"))
        Assert.IsTrue(settings.InExtensions |> List.exists (fun e -> e = ".scala"))
        ()

    [<Test>]
    member this.FindSettings_AddPatterns_HasPatterns () =
        let settings = FindSettings.DefaultSettings
        let settings = { settings with InFilePatterns = FindSettings.AddPattern "Find" settings.InFilePatterns }
        Assert.AreEqual(1, settings.InFilePatterns.Length)
        Assert.AreEqual("Find", settings.InFilePatterns.Head.ToString())
        ()

    [<Test>]
    member this.FindSettings_SetArchivesOnly_HasIncludeArchives () =
        let settings = FindSettings.SetArchivesOnly true FindSettings.DefaultSettings 
        Assert.IsTrue(settings.ArchivesOnly)
        Assert.IsTrue(settings.IncludeArchives)
        ()

    [<Test>]
    member this.FindSettings_SetDebug_HasVerbose () =
        let settings = FindSettings.SetDebug true FindSettings.DefaultSettings 
        Assert.IsTrue(settings.Debug)
        Assert.IsTrue(settings.Verbose)
        ()
