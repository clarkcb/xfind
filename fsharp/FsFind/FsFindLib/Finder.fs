namespace FsFind

open System
open System.IO
open System.Text.RegularExpressions

type Finder (settings : FindSettings.t) =
    let _fileTypes = FileTypes()

    // member methods
    member this.ValidateSettings () : string list =
        [
            (if List.isEmpty settings.Paths then (Some "Startpath not defined") else None);
            (if (List.exists (fun p -> not (Directory.Exists(p)) && not (File.Exists(p))) settings.Paths)
             then (Some "Startpath not found") else None);
        ]
        |> List.choose id

    member this.MatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        Seq.exists (fun p -> (p:Regex).Match(s).Success) patterns

    member this.AnyMatchesAnyPattern (slist : string seq) (patterns : Regex list) : bool =
        Seq.exists (fun s -> this.MatchesAnyPattern s patterns) slist

    member this.IsMatchingDir (d : DirectoryInfo) : bool =
        let elems = d.FullName.Split('/', '\\') |> Seq.filter (fun s -> not (String.IsNullOrEmpty s))
        (not settings.ExcludeHidden ||
         not (Seq.exists (FileUtil.IsHidden) elems)) &&
        (Seq.isEmpty settings.InDirPatterns ||
         this.AnyMatchesAnyPattern elems settings.InDirPatterns) &&
        (Seq.isEmpty settings.OutDirPatterns ||
         not (this.AnyMatchesAnyPattern elems settings.OutDirPatterns))

    member this.IsMatchingFile (f : FileResult.t) : bool =
        (List.isEmpty settings.InExtensions ||
         List.exists (fun x -> x = f.File.Extension) settings.InExtensions) &&
        (List.isEmpty settings.OutExtensions ||
         not (List.exists (fun x -> x = f.File.Extension) settings.OutExtensions)) &&
        (List.isEmpty settings.InFilePatterns ||
         List.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.InFilePatterns) &&
        (List.isEmpty settings.OutFilePatterns ||
         not (List.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.OutFilePatterns)) &&
        (List.isEmpty settings.InFileTypes ||
         List.exists (fun ft -> ft = f.FileType) settings.InFileTypes) &&
        (List.isEmpty settings.OutFileTypes ||
         not (List.exists (fun ft -> ft = f.FileType) settings.OutFileTypes))

    member this.IsMatchingArchiveFile (f : FileResult.t) : bool =
        (Seq.isEmpty settings.InArchiveExtensions ||
         Seq.exists (fun x -> x = f.File.Extension) settings.InArchiveExtensions) &&
        (Seq.isEmpty settings.OutArchiveExtensions ||
         not (Seq.exists (fun x -> x = f.File.Extension) settings.OutArchiveExtensions)) &&
        (Seq.isEmpty settings.InArchiveFilePatterns ||
         Seq.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.InArchiveFilePatterns) &&
        (Seq.isEmpty settings.OutArchiveFilePatterns ||
         not (Seq.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.OutArchiveFilePatterns))

    member this.FilterToFileResult (f: FileInfo) : FileResult.t Option = 
        if settings.ExcludeHidden && FileUtil.IsHiddenFile f then
            None
        else
            let fr = FileResult.Create f (_fileTypes.GetFileType f)
            if fr.FileType = FileType.Archive then
                if settings.IncludeArchives && this.IsMatchingArchiveFile fr then
                    Some fr
                else
                    None
            else
                if not settings.ArchivesOnly && this.IsMatchingFile fr then
                    Some fr
                else
                    None

    member this.GetFileResults (path : string) : FileResult.t list =
        let expandedPath = FileUtil.ExpandPath path
        if Directory.Exists(expandedPath) then
            let findOption =
                if settings.Recursive then SearchOption.AllDirectories
                else SearchOption.TopDirectoryOnly
            let dir = DirectoryInfo(expandedPath)
            dir.EnumerateFiles("*", findOption)
            |> Seq.filter (fun f -> f.Directory = null || this.IsMatchingDir(f.Directory))
            |> Seq.choose this.FilterToFileResult
            |> List.ofSeq
        else
            let fileInfo = FileInfo(expandedPath)
            [FileResult.Create fileInfo (_fileTypes.GetFileType fileInfo)]

    member this.SortFileResults (fileResults : FileResult.t list) : FileResult.t list =
        if settings.SortBy = SortBy.FileName then
            fileResults
            |> List.sortBy (fun (r : FileResult.t) -> r.File.Name, r.File.DirectoryName)
        else if settings.SortBy = SortBy.FileType then
            fileResults
            |> List.sortBy (fun (r : FileResult.t) -> r.FileType, r.File.DirectoryName, r.File.Name)
        else
            fileResults
            |> List.sortBy (fun (r : FileResult.t) -> r.File.DirectoryName, r.File.Name)

    member this.Find () : FileResult.t list =
        let fileResults = settings.Paths |> List.collect this.GetFileResults
        if settings.SortDescending then this.SortFileResults fileResults |> List.rev
        else this.SortFileResults fileResults

    member this.GetMatchingDirs (fileResults : FileResult.t list) : DirectoryInfo list = 
        fileResults
        |> Seq.map (fun f -> f.File.Directory)
        |> Seq.distinctBy (fun d -> d.FullName)
        |> List.ofSeq

    member this.PrintMatchingDirs (fileResults : FileResult.t list) : unit = 
        let dirs = this.GetMatchingDirs fileResults
        if dirs.Length > 0 then
            Common.Log $"\nMatching directories (%d{dirs.Length}):"
            for d in dirs do
                printfn $"%s{d.FullName}"
        else
            Common.Log "\nMatching directories: 0"


    member this.GetMatchingFiles (findFiles : FileResult.t list) : FileInfo list = 
        findFiles
        |> Seq.map (fun f -> f.File)
        |> List.ofSeq

    member this.PrintMatchingFiles (findFiles : FileResult.t list) : unit = 
        let files = this.GetMatchingFiles findFiles
        if files.Length > 0 then
            Common.Log $"\nMatching files (%d{files.Length}):"
            for f in files do
                printfn $"%s{f.FullName}"
        else
            Common.Log "\nMatching files: 0"

;;
