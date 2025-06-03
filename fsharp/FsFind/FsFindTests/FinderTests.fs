namespace FsFindTests

open System
open System.IO
open NUnit.Framework
open FsFindLib

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

    member this.GetXfindPath () : string =
        let xfindPath = Environment.GetEnvironmentVariable("XFIND_PATH")
        if xfindPath = null then
            Path.Join(FileUtil.GetHomePath(), "src", "xfind")
        else
            xfindPath

    member this.GetFsFindPath () : string =
        Path.Join(this.GetXfindPath(), "fsharp", "FsFind")

    member this.GetSettings () : FindSettings =
        let settings = FindSettings()
        settings.Paths <- ["."]
        settings
        
    member this.GetBinPath () : string =
        Path.Join(this.GetXfindPath(), "bin")
        


    //////////////////////////////////////////////////////////////
    // IsMatchingDir tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsMatchingDir_SingleDot_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.That(finder.IsMatchingDir(DirectoryInfo(".")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_DoubleDot_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.That(finder.IsMatchingDir(DirectoryInfo("..")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_IsHidden_False () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.That(finder.IsMatchingDir(DirectoryInfo(".git")), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingDir_IsHiddenIncludeHidden_True () =
        let settings = this.GetSettings()
        settings.IncludeHidden <- true
        let finder = Finder(settings)
        Assert.That(finder.IsMatchingDir(DirectoryInfo(".git")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_NoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        Assert.That(finder.IsMatchingDir(DirectoryInfo("/Users")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_MatchesInPattern_True () =
        let settings = this.GetSettings()
        settings.InDirPatterns <- settings.AddPattern "Find" settings.InDirPatterns 
        let finder = Finder(settings)
        Assert.That(finder.IsMatchingDir(DirectoryInfo("CsFind")))
        ()

    [<Test>]
    member this.TestIsMatchingDir_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        settings.OutDirPatterns <- settings.AddPattern "Find" settings.OutDirPatterns 
        let finder = Finder(settings)
        Assert.That(finder.IsMatchingDir(DirectoryInfo("CsFind")), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingDir_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        settings.InDirPatterns <- settings.AddPattern "FindFiles" settings.InDirPatterns 
        let finder = Finder(settings)
        Assert.That(finder.IsMatchingDir(DirectoryInfo("CsFind")), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingDir_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        settings.OutDirPatterns <- settings.AddPattern "FindFiles" settings.OutDirPatterns 
        let finder = Finder(settings)
        let dir = DirectoryInfo("CsFind")
        Assert.That(finder.IsMatchingDir(dir))
        ()


    //////////////////////////////////////////////////////////////
    // IsMatchingFile tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestIsMatchingFile_NoExtensionsNoPatterns_True () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_MatchesInExtension_True () =
        let settings = this.GetSettings()
        settings.InExtensions <- settings.AddExtensions "fs" settings.InExtensions 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_DoesNotMatchInExtension_False () =
        let settings = this.GetSettings()
        settings.InExtensions <- settings.AddExtensions "java" settings.InExtensions 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingFile_MatchesOutExtension_False () =
        let settings = this.GetSettings()
        settings.OutExtensions <- settings.AddExtensions "fs" settings.OutExtensions 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingFile_DoesNotMatchOutExtension_True () =
        let settings = this.GetSettings()
        settings.OutExtensions <- settings.AddExtensions "java" settings.OutExtensions 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_MatchesInPattern_True () =
        let settings = this.GetSettings()
        settings.InFilePatterns <- settings.AddPattern "Find" settings.InFilePatterns 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "Finder.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf))
        ()

    [<Test>]
    member this.TestIsMatchingFile_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        settings.InFilePatterns <- settings.AddPattern "Find" settings.InFilePatterns 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingFile_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        settings.OutFilePatterns <- settings.AddPattern "Find" settings.OutFilePatterns 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "Finder.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingFile_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        settings.OutFilePatterns <- settings.AddPattern "Find" settings.OutFilePatterns 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingFileResult(sf))
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
        Assert.That(finder.IsMatchingArchiveFileResult(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_MatchesInExtension_True () =
        let settings = this.GetSettings()
        settings.InArchiveExtensions <- settings.AddExtensions "zip" settings.InArchiveExtensions 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingArchiveFileResult(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_DoesNotMatchInExtension_False () =
        let settings = this.GetSettings()
        settings.InArchiveExtensions <- settings.AddExtensions "gz" settings.InArchiveExtensions 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingArchiveFileResult(sf), Is.False)
        ()


    [<Test>]
    member this.TestIsMatchingArchiveFile_MatchesOutExtension_False () =
        let settings = this.GetSettings()
        settings.OutArchiveExtensions <- settings.AddExtensions "zip" settings.OutArchiveExtensions 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingArchiveFileResult(sf), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_DoesNotMatchOutExtension_True () =
        let settings = this.GetSettings()
        settings.OutArchiveExtensions <- settings.AddExtensions "gz" settings.OutArchiveExtensions 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingArchiveFileResult(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_MatchesInPattern_True () =
        let settings = this.GetSettings()
        settings.InArchiveFilePatterns <- settings.AddPattern "arch" settings.InArchiveFilePatterns 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingArchiveFileResult(sf))
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_DoesNotMatchInPattern_False () =
        let settings = this.GetSettings()
        settings.InArchiveFilePatterns <- settings.AddPattern "archives" settings.InArchiveFilePatterns 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingArchiveFileResult(sf), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_MatchesOutPattern_False () =
        let settings = this.GetSettings()
        settings.OutArchiveFilePatterns <- settings.AddPattern "arch" settings.OutArchiveFilePatterns 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingArchiveFileResult(sf), Is.False)
        ()

    [<Test>]
    member this.TestIsMatchingArchiveFile_DoesNotMatchOutPattern_True () =
        let settings = this.GetSettings()
        settings.OutArchiveFilePatterns <- settings.AddPattern "archives" settings.OutArchiveFilePatterns 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let sf = FileResult.Create file (this.FileTypes.GetFileType(file))
        Assert.That(finder.IsMatchingArchiveFileResult(sf))
        ()

    //////////////////////////////////////////////////////////////
    // FilterToFileResult tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestFilterToFileResult_IsHidden_IsNone () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), ".gitignore"))
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsNone)
        ()

    [<Test>]
    member this.TestFilterToFileResult_IsHiddenIncludeHidden_IsSome () =
        let settings = this.GetSettings()
        settings.IncludeHidden <- true
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), ".gitignore"))
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_ArchiveNoIncludeArchives_IsNone () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsNone)
        ()

    [<Test>]
    member this.TestFilterToFileResult_ArchiveIncludeArchives_IsSome () =
        let settings = this.GetSettings()
        settings.IncludeArchives <- true
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_IsMatchingArchiveFile_IsSome () =
        let settings = this.GetSettings()
        settings.IncludeArchives <- true
        settings.InArchiveExtensions <- settings.AddExtensions "zip" settings.InArchiveExtensions 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_NotIsMatchingArchiveFile_IsNone () =
        let settings = this.GetSettings()
        settings.IncludeArchives <- true
        settings.OutArchiveExtensions <- settings.AddExtensions "zip" settings.OutArchiveExtensions 
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsNone)
        ()

    [<Test>]
    member this.TestFilterToFileResult_ArchiveFileArchivesOnly_IsSome () =
        let settings = this.GetSettings()
        settings.ArchivesOnly <- true
        let finder = Finder(settings)
        let file = FileInfo("archive.zip")
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_NoExtensionsNoPatterns_IsSome () =
        let settings = this.GetSettings()
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_MatchesInExtension_IsSome () =
        let settings = this.GetSettings()
        settings.InExtensions <- settings.AddExtensions "fs" settings.InExtensions 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsSome)
        ()

    [<Test>]
    member this.TestFilterToFileResult_MatchesOutExtension_IsNone () =
        let settings = this.GetSettings()
        settings.OutExtensions <- settings.AddExtensions "fs" settings.OutExtensions 
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsNone)
        ()

    [<Test>]
    member this.TestFilterToFileResult_NonArchiveFileArchivesOnly_IsNone () =
        let settings = this.GetSettings()
        settings.ArchivesOnly <- true
        let finder = Finder(settings)
        let file = FileInfo(Path.Join(this.GetFsFindPath(), "FsFindLib", "FileUtil.fs"))
        let fr = finder.FilterToFileResult file
        Assert.That(fr.IsNone)
        ()

    //////////////////////////////////////////////////////////////
    // FollowSymlinks tests
    //////////////////////////////////////////////////////////////
    [<Test>]
    member this.TestFollowSymlinks_Default_Excluded () =
        let settings = FindSettings()
        settings.Paths <- [this.GetBinPath()]
        let finder = Finder(settings)
        let fileResults = finder.Find()
        Assert.That(List.length fileResults, Is.LessThan(4))
        ()

    [<Test>]
    member this.TestFollowSymlinks_FollowSymlinks_Included () =
        let settings = FindSettings()
        settings.Paths <- [this.GetBinPath()]
        settings.FollowSymlinks <- true
        let finder = Finder(settings)
        let fileResults = finder.Find()
        Assert.That(List.length fileResults, Is.EqualTo(0).Or.GreaterThan(2))
        ()

    [<Test>]
    member this.TestFollowSymlinks_NoFollowSymlinks_Excluded () =
        let settings = FindSettings()
        settings.Paths <- [this.GetBinPath()]
        settings.FollowSymlinks <- false
        let finder = Finder(settings)
        let fileResults = finder.Find()
        Assert.That(List.length fileResults, Is.LessThan(4))
        ()
