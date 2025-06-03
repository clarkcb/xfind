namespace FsFindTests

open NUnit.Framework
open FsFindLib

[<TestFixture>]
type FindOptionsTests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.SettingsFromArgs_NoArgs_HasDefaultValues () =
        let args : string[] = [||]
        match FindOptions.SettingsFromArgs(args) with
        | Ok settings ->
            Assert.That(settings.ArchivesOnly, Is.False)
            Assert.That(settings.Debug, Is.False)
            Assert.That(settings.FollowSymlinks, Is.False)
            Assert.That(settings.IncludeArchives, Is.False)
            Assert.That(settings.IncludeHidden, Is.False)
            Assert.That(settings.Paths.Length, Is.EqualTo(0))
            Assert.That(settings.PrintDirs, Is.False)
            Assert.That(settings.PrintFiles)
            Assert.That(settings.PrintUsage, Is.False)
            Assert.That(settings.PrintVersion, Is.False)
            Assert.That(settings.Recursive)
            Assert.That(settings.SortBy, Is.EqualTo(SortBy.FilePath))
            Assert.That(settings.SortCaseInsensitive, Is.False)
            Assert.That(settings.SortDescending, Is.False)
            Assert.That(settings.Verbose, Is.False)
        | Error _ ->
            Assert.That(true, Is.False)
        ()

    [<Test>]
    member this.SettingsFromArgs_ValidArgs_HasArgValues () =
        let args = [| "-x"; "cs"; "." |]
        match FindOptions.SettingsFromArgs(args) with
        | Ok settings ->
            Assert.That(settings.InExtensions.Length, Is.EqualTo(1))
            Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".cs"))
            Assert.That(settings.Paths.Length, Is.EqualTo(1))
            Assert.That(settings.Paths.Head.ToString(), Is.EqualTo("."))
        | Error _ ->
            Assert.That(true, Is.False)
        ()

    [<Test>]
    member this.SettingsFromArgs_InValidArgs_ThrowsFindException () =
        let args = [| "-x"; "cs"; "."; "-Q" |]
        match FindOptions.SettingsFromArgs(args) with
        | Ok _ ->
            Assert.That(true, Is.False)
        | Error e ->
            Assert.That(e, Is.EqualTo("Invalid option: Q"))
        ()

    [<Test>]
    member this.SettingsFromJson_EqualsExpected () =
        let json = String.concat "" [
            "{";
            "\"path\": \"~/src/xfind/\",";
            "\"in-ext\": [\"js\", \"ts\"],"
            "\"out-dirpattern\": \"node_module\",";
            "\"out-filepattern\": [\"temp\"],"
            "\"debug\": true,";
            "\"followsymlinks\": true,";
            "\"includehidden\": true"
            "}"
        ]
        match FindOptions.SettingsFromJson(json) with
        | Ok settings ->
            Assert.That(settings.Paths.Length, Is.EqualTo(1))
            Assert.That(settings.Paths.Head.ToString(), Is.EqualTo("~/src/xfind/"))
            Assert.That(settings.InExtensions.Length, Is.EqualTo(2))
            Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".js"))
            Assert.That(settings.InExtensions |> List.exists (fun e -> e = ".ts"))
            Assert.That(settings.OutDirPatterns.Length, Is.EqualTo(1))
            Assert.That(settings.OutDirPatterns |> List.exists (fun p -> p.ToString() = "node_module"))
            Assert.That(settings.OutFilePatterns.Length, Is.EqualTo(1))
            Assert.That(settings.OutFilePatterns |> List.exists (fun p -> p.ToString() = "temp"))
            Assert.That(settings.Debug, Is.True)
            Assert.That(settings.FollowSymlinks, Is.True)
            Assert.That(settings.IncludeHidden, Is.True)
        | Error e ->
            Assert.That(true, Is.False)
        ()
