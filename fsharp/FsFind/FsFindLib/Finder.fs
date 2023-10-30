namespace FsFind

open System
open System.IO
open System.Text.RegularExpressions

type Finder (settings : FindSettings) =
    let _fileTypes = FileTypes()

    // member methods
    member this.ValidateSettings () : string list =
        [
            (if List.isEmpty settings.Paths then (Some "Startpath not defined") else None);
            (if (List.exists (fun p -> not (Directory.Exists(p)) && not (File.Exists(p))) settings.Paths)
             then (Some "Startpath not found") else None);
            (if settings.MaxDepth > -1 && settings.MinDepth > -1 && settings.MaxDepth < settings.MinDepth
             then (Some "Invalid range for mindepth and maxdepth") else None);
            (if settings.MaxLastMod.IsSome && settings.MinLastMod.IsSome && settings.MaxLastMod.Value < settings.MinLastMod.Value
             then (Some "Invalid range for minlastmod and maxlastmod") else None);
            (if settings.MaxSize > 0 && settings.MinSize > 0 && settings.MaxSize < settings.MinSize
             then (Some "Invalid range for minsize and maxsize") else None);
        ]
        |> List.choose id

    member this.MatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        Seq.exists (fun p -> (p:Regex).Match(s).Success) patterns

    member this.AnyMatchesAnyPattern (slist : string seq) (patterns : Regex list) : bool =
        Seq.exists (fun s -> this.MatchesAnyPattern s patterns) slist

    member this.IsMatchingDir (d : DirectoryInfo) : bool =
        let elems = d.FullName.Split('/', '\\') |> Seq.filter (fun s -> not (String.IsNullOrEmpty s))
        (settings.IncludeHidden ||
         not (Seq.exists FileUtil.IsHidden elems)) &&
        (Seq.isEmpty settings.InDirPatterns ||
         this.AnyMatchesAnyPattern elems settings.InDirPatterns) &&
        (Seq.isEmpty settings.OutDirPatterns ||
         not (this.AnyMatchesAnyPattern elems settings.OutDirPatterns))

    member this.IsMatchingFileResult (fr : FileResult.t) : bool =
        (List.isEmpty settings.InExtensions ||
         List.exists (fun x -> x = fr.File.Extension) settings.InExtensions) &&
        (List.isEmpty settings.OutExtensions ||
         not (List.exists (fun x -> x = fr.File.Extension) settings.OutExtensions)) &&
        (List.isEmpty settings.InFilePatterns ||
         List.exists (fun p -> (p:Regex).Match(fr.File.Name).Success) settings.InFilePatterns) &&
        (List.isEmpty settings.OutFilePatterns ||
         not (List.exists (fun p -> (p:Regex).Match(fr.File.Name).Success) settings.OutFilePatterns)) &&
        (List.isEmpty settings.InFileTypes ||
         List.exists (fun ft -> ft = fr.FileType) settings.InFileTypes) &&
        (List.isEmpty settings.OutFileTypes ||
         not (List.exists (fun ft -> ft = fr.FileType) settings.OutFileTypes)) &&
        (settings.MaxLastMod.IsNone || fr.File.LastWriteTimeUtc <= settings.MaxLastMod.Value) &&
        (settings.MaxSize = 0 || fr.File.Length <= settings.MaxSize) &&
        (settings.MinLastMod.IsNone || fr.File.LastWriteTimeUtc >= settings.MinLastMod.Value) &&
        (settings.MinSize = 0 || fr.File.Length >= settings.MinSize)

    member this.IsMatchingArchiveFile (fr : FileResult.t) : bool =
        (Seq.isEmpty settings.InArchiveExtensions ||
         Seq.exists (fun x -> x = fr.File.Extension) settings.InArchiveExtensions) &&
        (Seq.isEmpty settings.OutArchiveExtensions ||
         not (Seq.exists (fun x -> x = fr.File.Extension) settings.OutArchiveExtensions)) &&
        (Seq.isEmpty settings.InArchiveFilePatterns ||
         Seq.exists (fun p -> (p:Regex).Match(fr.File.Name).Success) settings.InArchiveFilePatterns) &&
        (Seq.isEmpty settings.OutArchiveFilePatterns ||
         not (Seq.exists (fun p -> (p:Regex).Match(fr.File.Name).Success) settings.OutArchiveFilePatterns))

    member this.FilterToFileResult (f: FileInfo) : FileResult.t Option = 
        if not settings.IncludeHidden && FileUtil.IsHiddenFile f then
            None
        else
            let fr = FileResult.Create f (_fileTypes.GetFileType f)
            if fr.FileType = FileType.Archive then
                if settings.IncludeArchives && this.IsMatchingArchiveFile fr then
                    Some fr
                else
                    None
            else
                if not settings.ArchivesOnly && this.IsMatchingFileResult fr then
                    Some fr
                else
                    None

    member this.MatchFile (f : FileInfo) (startPathSepCount : int) : bool =
        if f.Directory = null then
            true
        else
            let fileSepCount = FileUtil.SepCount f.FullName
            let depth = fileSepCount - startPathSepCount
            depth >= settings.MinDepth &&
            (settings.MaxDepth < 1 || depth <= settings.MaxDepth) &&
            this.IsMatchingDir f.Directory
        
    member this.GetFileResults (path : string) : FileResult.t list =
        let expandedPath = FileUtil.ExpandPath path
        let pathSepCount = FileUtil.SepCount path
        if Directory.Exists(expandedPath) then
            // if MaxDepth is zero, we can skip since a directory cannot be a result
            if settings.MaxDepth <> 0 then
                let findOption =
                    if settings.Recursive then SearchOption.AllDirectories
                    else SearchOption.TopDirectoryOnly
                let dir = DirectoryInfo(expandedPath)
                dir.EnumerateFiles("*", findOption)
                |> Seq.filter (fun f -> this.MatchFile f pathSepCount)
                |> Seq.choose this.FilterToFileResult
                |> List.ofSeq
            else
                []
        else
            // if MinDepth > zero, we can skip since the file is at depth zero
            if settings.MinDepth <= 0 then
                let fileInfo = FileInfo(expandedPath)
                [FileResult.Create fileInfo (_fileTypes.GetFileType fileInfo)]
            else
                []

    member this.SortByPath (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let cmp = if settings.SortCaseInsensitive then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
        let dirNameCmp = String.Compare(fr1.File.DirectoryName, fr2.File.DirectoryName, cmp)
        if dirNameCmp = 0 then String.Compare(fr1.File.Name, fr2.File.Name, cmp) else dirNameCmp

    member this.SortByName (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let cmp = if settings.SortCaseInsensitive then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
        let fileNameCmp = String.Compare(fr1.File.Name, fr2.File.Name, cmp)
        if fileNameCmp = 0 then String.Compare(fr1.File.DirectoryName, fr2.File.DirectoryName, cmp) else fileNameCmp

    member this.SortBySize (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let sizeCmp = fr1.File.Length.CompareTo(fr2.File.Length)
        if sizeCmp = 0 then (this.SortByPath fr1 fr2) else sizeCmp

    member this.SortByType (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let typeCmp = fr1.FileType.CompareTo(fr2.FileType)
        if typeCmp = 0 then (this.SortByPath fr1 fr2) else typeCmp

    member this.SortByLastMod (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let lastModCmp = fr1.File.LastWriteTimeUtc.CompareTo(fr2.File.LastWriteTimeUtc)
        if lastModCmp = 0 then (this.SortByPath fr1 fr2) else lastModCmp

    member this.SortFileResults (fileResults : FileResult.t list) : FileResult.t list =
        match settings.SortBy with
        | SortBy.FileName -> List.sortWith this.SortByName fileResults
        | SortBy.FileSize -> List.sortWith this.SortBySize fileResults
        | SortBy.FileType -> List.sortWith this.SortByType fileResults
        | SortBy.LastMod  -> List.sortWith this.SortByLastMod fileResults
        | _               -> List.sortWith this.SortByPath fileResults

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


    member this.GetMatchingFiles (fileResults : FileResult.t list) : FileInfo list = 
        fileResults
        |> Seq.map (fun f -> f.File)
        |> List.ofSeq

    member this.PrintMatchingFiles (fileResults : FileResult.t list) : unit = 
        let files = this.GetMatchingFiles fileResults
        if files.Length > 0 then
            Common.Log $"\nMatching files (%d{files.Length}):"
            for f in files do
                printfn $"%s{f.FullName}"
        else
            Common.Log "\nMatching files: 0"

;;
