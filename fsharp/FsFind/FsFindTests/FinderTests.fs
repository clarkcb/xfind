namespace FsFindTests

open System.IO
open NUnit.Framework
open FsFind

[<TestFixture>]
type FinderTests () =

    member this.FileTypes = FileTypes()

    [<SetUp>]
    member this.Setup () =
        ()

    member this.GetTestFileContent () : string =
        try
            EmbeddedTestResource.GetResourceFileContents "FsFindTests.Resources.testFile2.txt"
        with
        | :? IOException as e -> raise e
        | :? FindException as e -> raise e

    member this.GetSettings () : FindSettings.t =
        let settings = { FindSettings.DefaultSettings with Paths = ["."] }
        settings


    //////////////////////////////////////////////////////////////
    // IsMatchingDir tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsMatchingDir_SingleDot_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.True(finder.IsMatchingDir(DirectoryInfo(".")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_DoubleDot_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.True(finder.IsMatchingDir(DirectoryInfo("..")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_IsHidden_False () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.False(finder.IsMatchingDir(DirectoryInfo(".git")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_IsHiddenIncludeHidden_True () =
        let settings = { this.GetSettings() with ExcludeHidden = false }
        let finder = Finder(settings)
        Assert.True(finder.IsMatchingDir(DirectoryInfo(".git")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_NoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.True(finder.IsMatchingDir(DirectoryInfo("/Users")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InDirPatterns = FindSettings.AddPattern "Find" settings.InDirPatterns }
        let finder = Finder(settings)
        Assert.True(finder.IsMatchingDir(DirectoryInfo("CsFind")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutDirPatterns = FindSettings.AddPattern "Find" settings.OutDirPatterns }
        let finder = Finder(settings)
        Assert.False(finder.IsMatchingDir(DirectoryInfo("CsFind")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InDirPatterns = FindSettings.AddPattern "FindFiles" settings.InDirPatterns }
        let finder = Finder(settings)
        Assert.False(finder.IsMatchingDir(DirectoryInfo("CsFind")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutDirPatterns = FindSettings.AddPattern "FindFiles" settings.OutDirPatterns }
        let finder = Finder(settings)
        let dir = DirectoryInfo("CsFind")
        Assert.True(finder.IsMatchingDir(dir))
        ()


    //////////////////////////////////////////////////////////////
    // IsMatchingFile tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsMatchingFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_MatchesInExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = FindSettings.AddExtensions "cs" settings.InExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_DoesNotMatchInExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = FindSettings.AddExtensions "java" settings.InExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsMatchingFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_MatchesOutExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = FindSettings.AddExtensions "cs" settings.OutExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsMatchingFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_DoesNotMatchOutExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = FindSettings.AddExtensions "java" settings.OutExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InFilePatterns = FindSettings.AddPattern "Find" settings.InFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("Finder.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InFilePatterns = FindSettings.AddPattern "Find" settings.InFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsMatchingFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutFilePatterns = FindSettings.AddPattern "Find" settings.OutFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("Finder.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsMatchingFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutFilePatterns = FindSettings.AddPattern "Find" settings.OutFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingFile(sf))
        ()


    //////////////////////////////////////////////////////////////
    // IsMatchingArchiveFile tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsMatchingArchiveFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingArchiveFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_MatchesInExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveExtensions = FindSettings.AddExtensions "zip" settings.InArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingArchiveFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_DoesNotMatchInExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveExtensions = FindSettings.AddExtensions "gz" settings.InArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsMatchingArchiveFile(sf))
        ()


    [<Test>]
    member this.TestIsMatchingArchiveFile_MatchesOutExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveExtensions = FindSettings.AddExtensions "zip" settings.OutArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsMatchingArchiveFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_DoesNotMatchOutExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveExtensions = FindSettings.AddExtensions "gz" settings.OutArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingArchiveFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveFilePatterns = FindSettings.AddPattern "arch" settings.InArchiveFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingArchiveFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveFilePatterns = FindSettings.AddPattern "archives" settings.InArchiveFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsMatchingArchiveFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveFilePatterns = FindSettings.AddPattern "arch" settings.OutArchiveFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsMatchingArchiveFile(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveFilePatterns = FindSettings.AddPattern "archives" settings.OutArchiveFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsMatchingArchiveFile(sf))
        ()

    //////////////////////////////////////////////////////////////
    // FilterToFileResult tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestFilterToFileResult_IsHidden_IsNone () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo(".gitignore")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsNone)
        ()

    [<Test>]
    member this.TestFilterToFileResult_IsHiddenIncludeHidden_IsSome () =
        let settings = { this.GetSettings() with ExcludeHidden = false }
        let finder = Finder(settings)
        let file = FileInfo(".gitignore")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_ArchiveNoIncludeArchives_IsNone () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsNone)
        ()

    [<Test>]
    member this.TestFilterToFileResult_ArchiveIncludeArchives_IsSome () =
        let settings = { this.GetSettings() with IncludeArchives = true }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_IsMatchingArchiveFile_IsSome () =
        let settings = { this.GetSettings() with IncludeArchives = true }
        let settings = { settings with InArchiveExtensions = FindSettings.AddExtensions "zip" settings.InArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_NotIsMatchingArchiveFile_IsNone () =
        let settings = { this.GetSettings() with IncludeArchives = true }
        let settings = { settings with OutArchiveExtensions = FindSettings.AddExtensions "zip" settings.OutArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsNone)
        ()

    [<Test>]
    member this.TestFilterToFileResult_ArchiveFileArchivesOnly_IsSome () =
        let settings = { this.GetSettings() with ArchivesOnly = true; IncludeArchives = true }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_NoExtensionsNoPatterns_IsSome () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_MatchesInExtension_IsSome () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = FindSettings.AddExtensions "cs" settings.InExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_MatchesOutExtension_IsNone () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = FindSettings.AddExtensions "cs" settings.OutExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsNone)
        ()

    [<Test>]
    member this.TestFilterToFileResult_NonArchiveFileArchivesOnly_IsNone () =
        let settings = { this.GetSettings() with ArchivesOnly = true; IncludeArchives = true }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let fr = finder.FilterToFileResult file
        Assert.True(fr.IsNone)
        ()
