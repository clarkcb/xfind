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
    // IsFindDirectory tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsFindDirectory_SingleDot_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.True(finder.IsFindDir(DirectoryInfo(".")))
        ()

    [<Test>]
    member this.TestIsFindDirectory_DoubleDot_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.True(finder.IsFindDir(DirectoryInfo("..")))
        ()

    [<Test>]
    member this.TestIsFindDirectory_IsHidden_False () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.False(finder.IsFindDir(DirectoryInfo(".git")))
        ()

    [<Test>]
    member this.TestIsFindDirectory_IsHiddenIncludeHidden_True () =
        let settings = { this.GetSettings() with ExcludeHidden = false }
        let finder = Finder(settings)
        Assert.True(finder.IsFindDir(DirectoryInfo(".git")))
        ()

    [<Test>]
    member this.TestIsFindDirectory_NoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.True(finder.IsFindDir(DirectoryInfo("/Users")))
        ()

    [<Test>]
    member this.TestIsFindDirectory_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InDirPatterns = FindSettings.AddPattern "Find" settings.InDirPatterns }
        let finder = Finder(settings)
        Assert.True(finder.IsFindDir(DirectoryInfo("CsFind")))
        ()

    [<Test>]
    member this.TestIsFindDirectory_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutDirPatterns = FindSettings.AddPattern "Find" settings.OutDirPatterns }
        let finder = Finder(settings)
        Assert.False(finder.IsFindDir(DirectoryInfo("CsFind")))
        ()

    [<Test>]
    member this.TestIsFindDirectory_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InDirPatterns = FindSettings.AddPattern "FindFiles" settings.InDirPatterns }
        let finder = Finder(settings)
        Assert.False(finder.IsFindDir(DirectoryInfo("CsFind")))
        ()

    [<Test>]
    member this.TestIsFindDirectory_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutDirPatterns = FindSettings.AddPattern "FindFiles" settings.OutDirPatterns }
        let finder = Finder(settings)
        let dir = DirectoryInfo("CsFind")
        Assert.True(finder.IsFindDir(dir))
        ()


    //////////////////////////////////////////////////////////////
    // IsFindFile tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsFindFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsFindFile(sf))
        ()

    [<Test>]
    member this.TestIsFindFile_MatchesInExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = FindSettings.AddExtensions "cs" settings.InExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsFindFile(sf))
        ()

    [<Test>]
    member this.TestIsFindFile_DoesNotMatchInExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = FindSettings.AddExtensions "java" settings.InExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsFindFile(sf))
        ()


    [<Test>]
    member this.TestIsFindFile_MatchesOutExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = FindSettings.AddExtensions "cs" settings.OutExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsFindFile(sf))
        ()

    [<Test>]
    member this.TestIsFindFile_DoesNotMatchOutExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = FindSettings.AddExtensions "java" settings.OutExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsFindFile(sf))
        ()

    [<Test>]
    member this.TestIsFindFile_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InFilePatterns = FindSettings.AddPattern "Find" settings.InFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("Finder.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsFindFile(sf))
        ()

    [<Test>]
    member this.TestIsFindFile_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InFilePatterns = FindSettings.AddPattern "Find" settings.InFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsFindFile(sf))
        ()

    [<Test>]
    member this.TestIsFindFile_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutFilePatterns = FindSettings.AddPattern "Find" settings.OutFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("Finder.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsFindFile(sf))
        ()

    [<Test>]
    member this.TestIsFindFile_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutFilePatterns = FindSettings.AddPattern "Find" settings.OutFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsFindFile(sf))
        ()


    //////////////////////////////////////////////////////////////
    // IsArchiveFindFile tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsArchiveFindFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsArchiveFindFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveFindFile_MatchesInExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveExtensions = FindSettings.AddExtensions "zip" settings.InArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsArchiveFindFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveFindFile_DoesNotMatchInExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveExtensions = FindSettings.AddExtensions "gz" settings.InArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsArchiveFindFile(sf))
        ()


    [<Test>]
    member this.TestIsArchiveFindFile_MatchesOutExtension_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveExtensions = FindSettings.AddExtensions "zip" settings.OutArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsArchiveFindFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveFindFile_DoesNotMatchOutExtension_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveExtensions = FindSettings.AddExtensions "gz" settings.OutArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsArchiveFindFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveFindFile_MatchesInPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveFilePatterns = FindSettings.AddPattern "arch" settings.InArchiveFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsArchiveFindFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveFindFile_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with InArchiveFilePatterns = FindSettings.AddPattern "archives" settings.InArchiveFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsArchiveFindFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveFindFile_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveFilePatterns = FindSettings.AddPattern "arch" settings.OutArchiveFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.IsArchiveFindFile(sf))
        ()

    [<Test>]
    member this.TestIsArchiveFindFile_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        let settings = { settings with OutArchiveFilePatterns = FindSettings.AddPattern "archives" settings.OutArchiveFilePatterns }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.IsArchiveFindFile(sf))
        ()

    //////////////////////////////////////////////////////////////
    // FilterFile tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestFilterFile_IsHidden_False () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo(".gitignore")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_IsHiddenIncludeHidden_True () =
        let settings = { this.GetSettings() with ExcludeHidden = false }
        let finder = Finder(settings)
        let file = FileInfo(".gitignore")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_ArchiveNoIncludeArchives_False () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_ArchiveIncludeArchives_True () =
        let settings = { this.GetSettings() with IncludeArchives = true }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_IsArchiveFindFile_True () =
        let settings = { this.GetSettings() with IncludeArchives = true }
        let settings = { settings with InArchiveExtensions = FindSettings.AddExtensions "zip" settings.InArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_NotIsArchiveFindFile_False () =
        let settings = { this.GetSettings() with IncludeArchives = true }
        let settings = { settings with OutArchiveExtensions = FindSettings.AddExtensions "zip" settings.OutArchiveExtensions }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_ArchiveFileArchivesOnly_True () =
        let settings = { this.GetSettings() with ArchivesOnly = true }
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.FilterFile(sf))
        ()


    [<Test>]
    member this.TestFilterFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_IsFindFile_True () =
        let settings = this.GetSettings()
        let settings = { settings with InExtensions = FindSettings.AddExtensions "cs" settings.InExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.True(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_NotIsFindFile_False () =
        let settings = this.GetSettings()
        let settings = { settings with OutExtensions = FindSettings.AddExtensions "cs" settings.OutExtensions }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.FilterFile(sf))
        ()

    [<Test>]
    member this.TestFilterFile_NonArchiveFileArchivesOnly_False () =
        let settings = { this.GetSettings() with ArchivesOnly = true }
        let finder = Finder(settings)
        let file = FileInfo("FileUtil.cs")
        let sf = FindFile.Create file (this.FileTypes.GetFileType(file))
        Assert.False(finder.FilterFile(sf))
        ()
