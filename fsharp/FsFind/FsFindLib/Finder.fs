namespace FsFindLib

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

    member this.FilterDirByHidden (d : DirectoryInfo) : bool =
        (settings.IncludeHidden ||
         not (FileUtil.IsHiddenDirectory d))

    member this.FilterDirByInPatterns (d : DirectoryInfo) : bool =
        let elems = FileUtil.GetDirElems(d)
        (Seq.isEmpty settings.InDirPatterns ||
         this.AnyMatchesAnyPattern elems settings.InDirPatterns)

    member this.FilterDirByOutPatterns (d : DirectoryInfo) : bool =
        let elems = FileUtil.GetDirElems(d)
        (Seq.isEmpty settings.OutDirPatterns ||
         not (this.AnyMatchesAnyPattern elems settings.OutDirPatterns))

    member this.IsMatchingDir (d : DirectoryInfo) : bool =
        if not settings.FollowSymlinks && d.Exists && d.Attributes.HasFlag(FileAttributes.ReparsePoint) then
            false
        else
            this.FilterDirByHidden(d) && this.FilterDirByInPatterns(d) && this.FilterDirByOutPatterns(d)

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
        elif not (this.IsMatchingDir(f.Directory)) then
            None
        elif not settings.IncludeHidden && FileUtil.IsHiddenName f.Name then
            None
        else
            let fr = FileResult.Create f (_fileTypes.GetFileType f)
            if fr.FileType = FileType.Archive then
                if settings.IncludeArchives && this.IsMatchingArchiveFileResult fr then
                    Some fr
                else
                    None
            elif not settings.ArchivesOnly && this.IsMatchingFileResult fr then
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
                    |> Seq.filter (fun d -> this.FilterDirByHidden(d) && this.FilterDirByOutPatterns(d))
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
                if (this.FilterDirByHidden dir) && (this.FilterDirByOutPatterns dir) then
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

    member this.Find () : FileResult.t list =
        this.SetEnumerationOptions()
        let fileResults = settings.Paths |> List.collect this.GetFileResults
        let fileResultSorter = FileResultSorter(settings)
        fileResultSorter.Sort fileResults

    member this.GetMatchingDirs (fileResults : FileResult.t list) : DirectoryInfo list = 
        fileResults
        |> Seq.map (fun f -> f.File.Directory)
        |> Seq.distinctBy (fun d -> d.FullName)
        |> List.ofSeq

    member this.PrintMatchingDirs (fileResults : FileResult.t list) (formatter : FileResultFormatter) : unit = 
        let dirs = this.GetMatchingDirs fileResults
        if dirs.Length > 0 then
            Logger.Log $"\nMatching directories (%d{dirs.Length}):"
            for d in dirs do
                printfn $"%s{formatter.FormatDirectory(d)}"
        else
            Logger.Log "\nMatching directories: 0"

    member this.PrintMatchingFiles (fileResults : FileResult.t list) (formatter : FileResultFormatter) : unit = 
        if fileResults.Length > 0 then
            Logger.Log $"\nMatching files (%d{fileResults.Length}):"
            for fr in fileResults do
                printfn $"%s{formatter.FormatFileResult(fr)}"
        else
            Logger.Log "\nMatching files: 0"

;;
