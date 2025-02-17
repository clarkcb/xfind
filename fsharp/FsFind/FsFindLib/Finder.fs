namespace FsFind

open System
open System.IO
open System.Text.RegularExpressions

type Finder (settings : FindSettings) =
    let _fileTypes = FileTypes()
    let _enumerationOptions = EnumerationOptions()

    // member methods
    member this.ValidateSettings () : string list =
        [
            (if List.isEmpty settings.Paths then (Some "Startpath not defined") else None);
            (if (List.exists (fun p -> not (FileUtil.Exists(p))) settings.Paths)
             then (Some "Startpath not found") else None);
            (if settings.MaxDepth > -1 && settings.MinDepth > -1 && settings.MaxDepth < settings.MinDepth
             then (Some "Invalid range for mindepth and maxdepth") else None);
            (if settings.MaxLastMod.IsSome && settings.MinLastMod.IsSome && settings.MaxLastMod.Value < settings.MinLastMod.Value
             then (Some "Invalid range for minlastmod and maxlastmod") else None);
            (if settings.MaxSize > 0 && settings.MinSize > 0 && settings.MaxSize < settings.MinSize
             then (Some "Invalid range for minsize and maxsize") else None);
        ]
        |> List.choose id

    member this.SetEnumerationOptions () : unit =
        _enumerationOptions.AttributesToSkip <- FileAttributes.System
        _enumerationOptions.IgnoreInaccessible <- true
        _enumerationOptions.MatchType <- MatchType.Simple
        // Set recursion to false because we will do it manually
        _enumerationOptions.RecurseSubdirectories <- false
        _enumerationOptions.ReturnSpecialDirectories <- false
        if not settings.IncludeArchives then
            _enumerationOptions.AttributesToSkip <- _enumerationOptions.AttributesToSkip ||| FileAttributes.Compressed
        if not settings.IncludeHidden then
            _enumerationOptions.AttributesToSkip <- _enumerationOptions.AttributesToSkip ||| FileAttributes.Hidden

    member this.MatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        Seq.exists (fun p -> (p:Regex).Match(s).Success) patterns

    member this.AnyMatchesAnyPattern (slist : string seq) (patterns : Regex list) : bool =
        Seq.exists (fun s -> this.MatchesAnyPattern s patterns) slist

    member this.IsMatchingDir (d : DirectoryInfo) : bool =
        if not settings.FollowSymlinks && d.Exists && d.Attributes.HasFlag(FileAttributes.ReparsePoint) then
            false
        else
            let elems = FileUtil.GetDirElems(d)
            (settings.IncludeHidden ||
             not (Seq.exists FileUtil.IsHidden elems)) &&
            (Seq.isEmpty settings.InDirPatterns ||
             this.AnyMatchesAnyPattern elems settings.InDirPatterns) &&
            (Seq.isEmpty settings.OutDirPatterns ||
             not (this.AnyMatchesAnyPattern elems settings.OutDirPatterns))

    member this.IsMatchingArchiveExtension (ext : string) : bool =
        (List.isEmpty settings.InArchiveExtensions ||
         List.exists (fun x -> x = ext) settings.InArchiveExtensions) &&
        (List.isEmpty settings.OutArchiveExtensions ||
         not (List.exists (fun x -> x = ext) settings.OutArchiveExtensions))

    member this.IsMatchingExtension (ext : string) : bool =
        (List.isEmpty settings.InExtensions ||
         List.exists (fun x -> x = ext) settings.InExtensions) &&
        (List.isEmpty settings.OutExtensions ||
         not (List.exists (fun x -> x = ext) settings.OutExtensions))

    member this.IsMatchingArchiveFileName (fileName : string) : bool =
        (List.isEmpty settings.InArchiveFilePatterns ||
         List.exists (fun p -> (p:Regex).Match(fileName).Success) settings.InArchiveFilePatterns) &&
        (List.isEmpty settings.OutArchiveFilePatterns ||
         not (List.exists (fun p -> (p:Regex).Match(fileName).Success) settings.OutArchiveFilePatterns))

    member this.IsMatchingFileName (fileName : string) : bool =
        (List.isEmpty settings.InFilePatterns ||
         List.exists (fun p -> (p:Regex).Match(fileName).Success) settings.InFilePatterns) &&
        (List.isEmpty settings.OutFilePatterns ||
         not (List.exists (fun p -> (p:Regex).Match(fileName).Success) settings.OutFilePatterns))

    member this.IsMatchingFileType (fileType : FileType) : bool =
        (List.isEmpty settings.InFileTypes ||
         List.exists (fun ft -> ft = fileType) settings.InFileTypes) &&
        (List.isEmpty settings.OutFileTypes ||
         not (List.exists (fun ft -> ft = fileType) settings.OutFileTypes))

    member this.IsMatchingFileSize (fileSize : int64) : bool =
        (settings.MinSize = 0 || fileSize >= settings.MinSize) &&
        (settings.MaxSize = 0 || fileSize <= settings.MaxSize)

    member this.IsMatchingLastMod (lastMod : DateTime) : bool =
        (settings.MinLastMod.IsNone || lastMod >= settings.MinLastMod.Value) &&
        (settings.MaxLastMod.IsNone || lastMod <= settings.MaxLastMod.Value)

    member this.IsMatchingFileResult (fr : FileResult.t) : bool =
        this.IsMatchingExtension(fr.File.Extension) &&
        this.IsMatchingFileName(fr.File.Name) &&
        this.IsMatchingFileType(fr.FileType) &&
        this.IsMatchingFileSize(fr.File.Length) &&
        this.IsMatchingLastMod(fr.File.LastWriteTimeUtc)

    member this.IsMatchingArchiveFileResult (fr : FileResult.t) : bool =
        this.IsMatchingArchiveExtension(fr.File.Extension) &&
        this.IsMatchingArchiveFileName(fr.File.Name)

    member this.FilterToFileResult (f: FileInfo) : FileResult.t Option =
        if not settings.FollowSymlinks && f.Exists && f.Attributes.HasFlag(FileAttributes.ReparsePoint) then
            None
        else
            if not settings.IncludeHidden && FileUtil.IsHiddenFile f then
                None
            else
                let fr = FileResult.Create f (_fileTypes.GetFileType f)
                if fr.FileType = FileType.Archive then
                    if settings.IncludeArchives && this.IsMatchingArchiveFileResult fr then
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
   
    member this.RecGetFileResults (dir : DirectoryInfo) (minDepth : int) (maxDepth : int) (currentDepth : int) : FileResult.t list =
        if maxDepth > -1 && currentDepth > maxDepth then
            []
        else
            let fileResults =
                if minDepth < 0 || currentDepth >= minDepth then
                    dir.EnumerateFiles("*", _enumerationOptions)
                    |> Seq.choose this.FilterToFileResult
                    |> List.ofSeq
                else []
            let dirResults =
                if maxDepth < 0 || currentDepth < maxDepth then
                    dir.EnumerateDirectories()
                    |> Seq.filter this.IsMatchingDir
                    |> Seq.map (fun d -> this.RecGetFileResults d minDepth maxDepth (currentDepth + 1))
                    |> Seq.collect id
                    |> List.ofSeq
                else []
            List.concat [fileResults; dirResults]
   
    member this.GetFileResults (filePath : string) : FileResult.t list =
        let fp =
            if Directory.Exists(filePath) || File.Exists(filePath)
            then filePath
            else FileUtil.ExpandPath(filePath)
        if Directory.Exists(fp) then
            // if MaxDepth is zero, we can skip since a directory cannot be a result
            if settings.MaxDepth <> 0 then
                let dir = DirectoryInfo(fp)
                if this.IsMatchingDir dir then
                    let maxDepth = if settings.Recursive then settings.MaxDepth else 1
                    this.RecGetFileResults dir settings.MinDepth maxDepth 1
                else
                    []
            else
                []
        else
            // if MinDepth > zero, we can skip since the file is at depth zero
            if settings.MinDepth <= 0 then
                let fileInfo = FileInfo(fp)
                let fileResult = this.FilterToFileResult fileInfo
                if fileResult.IsSome then
                    [fileResult.Value]
                else
                    []
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
        this.SetEnumerationOptions()
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
            Logger.Log $"\nMatching directories (%d{dirs.Length}):"
            for d in dirs do
                printfn $"%s{d.FullName}"
        else
            Logger.Log "\nMatching directories: 0"


    member this.GetMatchingFiles (fileResults : FileResult.t list) : FileInfo list = 
        fileResults
        |> Seq.map (fun f -> f.File)
        |> List.ofSeq

    member this.PrintMatchingFiles (fileResults : FileResult.t list) : unit = 
        let files = this.GetMatchingFiles fileResults
        if files.Length > 0 then
            Logger.Log $"\nMatching files (%d{files.Length}):"
            for f in files do
                printfn $"%s{f.FullName}"
        else
            Logger.Log "\nMatching files: 0"

;;
