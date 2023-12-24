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
        Assert.That(settings.ArchivesOnly, Is.False)
        Assert.That(settings.Debug, Is.False)
        Assert.That(settings.IncludeArchives, Is.False)
        Assert.That(settings.IncludeHidden, Is.False)
        Assert.That(settings.ListDirs, Is.False)
        Assert.That(settings.ListFiles)
        Assert.That(settings.Paths.Length, Is.EqualTo(0))
        Assert.That(settings.PrintUsage, Is.False)
        Assert.That(settings.PrintVersion, Is.False)
        Assert.That(settings.Recursive)
        Assert.That(settings.SortBy, Is.EqualTo(SortBy.FilePath))
        Assert.That(settings.SortCaseInsensitive, Is.False)
        Assert.That(settings.SortDescending, Is.False)
        Assert.That(settings.Verbose, Is.False)
        ()

    [<Test>]
    member this.SettingsFromArgs_ValidArgs_HasArgValues () =
        let args = [| "-x"; "cs"; "." |]
        let settings, _ = FindOptions.SettingsFromArgs(args)
        Assert.That(settings.InExtensions.Length, Is.EqualTo(1))
        Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".cs"))
        Assert.That(settings.Paths.Length, Is.EqualTo(1))
        Assert.That(settings.Paths.Head.ToString(), Is.EqualTo("."))
        ()

    [<Test>]
    member this.SettingsFromArgs_InValidArgs_ThrowsFindException () =
        let args = [| "-x"; "cs"; "."; "-Q" |]
        let _, err = FindOptions.SettingsFromArgs(args)
        Assert.That(err, Is.EqualTo("Invalid option: Q"))
        ()
