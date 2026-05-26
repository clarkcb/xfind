namespace FsFindLib

open System
open System.IO
open System.Text.RegularExpressions

type Finder (settings : FindSettings) =
    let _fileTypes = FileTypes()
    let _enumerationOptions = EnumerationOptions()

    // member methods
    member this.ValidatePaths (paths : string list) : string list =
        if List.isEmpty paths
        then ["Startpath not defined"]
        elif List.exists (fun p -> not (FileUtil.Exists(p))) paths
        then ["Startpath not found"]
        elif not settings.FollowSymlinks && not (List.isEmpty (paths |> List.filter FileUtil.IsSymlink))
        then ["Startpath does not match find settings"]
        elif (not (List.isEmpty (List.filter FileUtil.IsDirectory paths))
              && List.exists (fun d -> (not (this.IsTraversableDir(DirectoryInfo(d))))) paths)
        then ["Startpath does not match find settings"]
        elif (not (List.isEmpty (List.filter FileUtil.IsFile paths))
              && (List.exists (fun f -> this.FilterFilePathToFileResult(f).IsNone) (List.filter FileUtil.IsFile paths)))
        then ["Startpath does not match find settings"]
        else []

    member this.ValidateSettings () : string list =
        match this.ValidatePaths settings.Paths with
            | [] ->
                if settings.MaxDepth > -1 && settings.MinDepth > -1 && settings.MaxDepth < settings.MinDepth
                then ["Invalid range for mindepth and maxdepth"]
                elif settings.MaxLastMod.IsSome && settings.MinLastMod.IsSome && settings.MaxLastMod.Value < settings.MinLastMod.Value
                then ["Invalid range for minlastmod and maxlastmod"]
                elif settings.MaxSize > 0 && settings.MinSize > 0 && settings.MaxSize < settings.MinSize
                then ["Invalid range for minsize and maxsize"]
                else []
            | errs -> errs

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

    member this.EmptyOrMatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        List.isEmpty patterns || this.MatchesAnyPattern s patterns

    member this.EmptyOrNotMatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        List.isEmpty patterns || not (this.MatchesAnyPattern s patterns)

    member this.EmptyOrAnyMatchesAnyPattern (slist : string seq) (patterns : Regex list) : bool =
        List.isEmpty patterns || this.AnyMatchesAnyPattern slist patterns

    member this.EmptyOrNotAnyMatchesAnyPattern (slist : string seq) (patterns : Regex list) : bool =
        List.isEmpty patterns || not (this.AnyMatchesAnyPattern slist patterns)

    member this.EmptyOrMatchesAnyString (s : string) (slist : string list) : bool =
        List.isEmpty slist || List.exists (fun x -> x = s) slist

    member this.EmptyOrNotMatchesAnyString (s : string) (slist : string list) : bool =
        List.isEmpty slist || not (List.exists (fun x -> x = s) slist)

    member this.EmptyOrMatchesAnyFileType (fileType : FileType) (fileTypes : FileType list) : bool =
        List.isEmpty fileTypes || List.exists (fun ft -> ft = fileType) fileTypes

    member this.EmptyOrNotMatchesAnyFileType (fileType : FileType) (fileTypes : FileType list) : bool =
        List.isEmpty fileTypes || not (List.exists (fun ft -> ft = fileType) fileTypes)

    member this.IsMatchingDirByHidden (d : DirectoryInfo) : bool =
        (settings.IncludeHidden ||
         not (FileUtil.IsHiddenDirectory d))

    member this.IsMatchingDirPathByHidden (dirPath : string) : bool =
        this.IsMatchingDirByHidden(DirectoryInfo(dirPath))

    member this.IsMatchingDirByInPatterns (d : DirectoryInfo) : bool =
        let elems = FileUtil.GetDirElems(d)
        this.EmptyOrAnyMatchesAnyPattern elems settings.InDirPatterns

    member this.IsMatchingDirByOutPatterns (d : DirectoryInfo) : bool =
        let elems = FileUtil.GetDirElems(d)
        this.EmptyOrNotAnyMatchesAnyPattern elems settings.OutDirPatterns

    member this.IsTraversableDir (d : DirectoryInfo) : bool =
        this.IsMatchingDirByHidden(d) &&
        this.IsMatchingDirByOutPatterns(d)

    member this.IsMatchingDir (d : DirectoryInfo) : bool =
        this.IsMatchingDirByHidden(d) &&
        this.IsMatchingDirByInPatterns(d) &&
        this.IsMatchingDirByOutPatterns(d)

    member this.IsMatchingArchiveExtension (ext : string) : bool =
        this.EmptyOrMatchesAnyString ext settings.InArchiveExtensions &&
        this.EmptyOrNotMatchesAnyString ext settings.OutArchiveExtensions

    member this.IsMatchingExtension (ext : string) : bool =
        this.EmptyOrMatchesAnyString ext settings.InExtensions &&
        this.EmptyOrNotMatchesAnyString ext settings.OutExtensions

    member this.IsMatchingArchiveFileName (fileName : string) : bool =
        this.EmptyOrMatchesAnyPattern fileName settings.InArchiveFilePatterns &&
        this.EmptyOrNotMatchesAnyPattern fileName settings.OutArchiveFilePatterns

    member this.IsMatchingFileName (fileName : string) : bool =
        this.EmptyOrMatchesAnyPattern fileName settings.InFilePatterns &&
        this.EmptyOrNotMatchesAnyPattern fileName settings.OutFilePatterns

    member this.IsMatchingFileType (fileType : FileType) : bool =
        this.EmptyOrMatchesAnyFileType fileType settings.InFileTypes &&
        this.EmptyOrNotMatchesAnyFileType fileType settings.OutFileTypes

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

    member this.FilterFileInfoToFileResult (f: FileInfo) : FileResult.t Option =
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
    
    member this.FilterFilePathToFileResult (filePath: string) : FileResult.t Option =
        this.FilterFileInfoToFileResult(FileInfo(filePath))

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
                    |> Seq.choose this.FilterFileInfoToFileResult
                    |> List.ofSeq
                else []
            let dirResults =
                if maxDepth < 0 || currentDepth < maxDepth then
                    dir.EnumerateDirectories()
                    |> Seq.filter this.IsTraversableDir
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
                if (this.IsMatchingDirByHidden dir) && (this.IsMatchingDirByOutPatterns dir) then
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
                let fileResult = this.FilterFileInfoToFileResult fileInfo
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
