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
        then [FindError.StartpathNotDefined]
        elif List.exists (fun p -> not (FileUtil.Exists(p))) paths
        then [FindError.StartpathNotFound]
        elif not settings.FollowSymlinks && not (List.isEmpty (paths |> List.filter FileUtil.IsSymlink))
        then [FindError.StartpathNotMatchFindSettings]
        elif (not (List.isEmpty (List.filter FileUtil.IsDirectory paths))
              && List.exists (fun d -> (not (this.IsTraversableDirPath(d)))) paths)
        then [FindError.StartpathNotMatchFindSettings]
        elif (not (List.isEmpty (List.filter FileUtil.IsFile paths))
              && (List.exists (fun f -> this.FilterFilePathToFileResult(f).IsNone) (List.filter FileUtil.IsFile paths)))
        then [FindError.StartpathNotMatchFindSettings]
        else []

    member this.ValidateSettings () : string list =
        match this.ValidatePaths settings.Paths with
            | [] ->
                if settings.MaxDepth > -1 && settings.MinDepth > -1 && settings.MaxDepth < settings.MinDepth
                then [FindError.InvalidRangeMinDepthMaxDepth]
                elif settings.MaxLastMod.IsSome && settings.MinLastMod.IsSome && settings.MaxLastMod.Value < settings.MinLastMod.Value
                then [FindError.InvalidRangeMinLastModMaxLastMod]
                elif settings.MaxSize > 0 && settings.MinSize > 0 && settings.MaxSize < settings.MinSize
                then [FindError.InvalidRangeMinSizeMaxSize]
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

    member this.AnyMatchesAnyPattern (sList: string seq) (patterns : Regex list) : bool =
        Seq.exists (fun s -> this.MatchesAnyPattern s patterns) sList

    member this.EmptyOrMatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        List.isEmpty patterns || this.MatchesAnyPattern s patterns

    member this.EmptyOrNotMatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        List.isEmpty patterns || not (this.MatchesAnyPattern s patterns)

    member this.EmptyOrAnyMatchesAnyPattern (sList: string seq) (patterns : Regex list) : bool =
        List.isEmpty patterns || this.AnyMatchesAnyPattern sList patterns

    member this.EmptyOrNotAnyMatchesAnyPattern (sList: string seq) (patterns : Regex list) : bool =
        List.isEmpty patterns || not (this.AnyMatchesAnyPattern sList patterns)

    member this.EmptyOrMatchesAnyString (s : string) (sList: string list) : bool =
        List.isEmpty sList || List.exists (fun x -> x = s) sList

    member this.EmptyOrNotMatchesAnyString (s : string) (sList: string list) : bool =
        List.isEmpty sList || not (List.exists (fun x -> x = s) sList)

    member this.EmptyOrMatchesAnyFileType (fileType : FileType) (fileTypes : FileType list) : bool =
        List.isEmpty fileTypes || List.exists (fun ft -> ft = fileType) fileTypes

    member this.EmptyOrNotMatchesAnyFileType (fileType : FileType) (fileTypes : FileType list) : bool =
        List.isEmpty fileTypes || not (List.exists (fun ft -> ft = fileType) fileTypes)

    member this.IsMatchingDirPathByHidden (dirPath : string) : bool =
        (settings.IncludeHidden || not (FileUtil.IsHiddenPath dirPath))

    member this.IsMatchingDirPathByInPatterns (dirPath: string) : bool =
        let elems = FileUtil.GetDirPathElems(dirPath)
        this.EmptyOrAnyMatchesAnyPattern elems settings.InDirPatterns

    member this.IsMatchingDirPathByOutPatterns (dirPath: string) : bool =
        let elems = FileUtil.GetDirPathElems(dirPath)
        this.EmptyOrNotAnyMatchesAnyPattern elems settings.OutDirPatterns

    member this.IsTraversableDirPath (dirPath: string) : bool =
        this.IsMatchingDirPathByHidden(dirPath) &&
        this.IsMatchingDirPathByOutPatterns(dirPath)

    member this.IsMatchingDirPath (dirPath: string) : bool =
        this.IsMatchingDirPathByHidden(dirPath) &&
        this.IsMatchingDirPathByInPatterns(dirPath) &&
        this.IsMatchingDirPathByOutPatterns(dirPath)

    member this.IsNullOrMatchingDirPath (dirPath: string) : bool =
        dirPath = null || this.IsMatchingDirPath(dirPath)

    member this.IsMatchingFileNameByHidden (fileName: string) : bool =
        (settings.IncludeHidden || not (FileUtil.IsHiddenName fileName))

    member this.IsMatchingArchiveExtension (ext : string) : bool =
        this.EmptyOrMatchesAnyString ext settings.InArchiveExtensions &&
        this.EmptyOrNotMatchesAnyString ext settings.OutArchiveExtensions

    member this.IsMatchingArchiveFileName (fileName : string) : bool =
        this.EmptyOrMatchesAnyPattern fileName settings.InArchiveFilePatterns &&
        this.EmptyOrNotMatchesAnyPattern fileName settings.OutArchiveFilePatterns

    member this.IsMatchingArchiveFilePath (filePath: string) : bool =
        this.IsMatchingArchiveExtension (FileUtil.GetFilePathExtension filePath) &&
        this.IsMatchingArchiveFileName (Path.GetFileName filePath)

    member this.IsMatchingArchiveFileResult (fr : FileResult.t) : bool =
        this.IsMatchingArchiveFilePath(fr.FilePath)

    member this.IsMatchingExtension (ext : string) : bool =
        this.EmptyOrMatchesAnyString ext settings.InExtensions &&
        this.EmptyOrNotMatchesAnyString ext settings.OutExtensions

    member this.IsMatchingFileName (fileName : string) : bool =
        this.EmptyOrMatchesAnyPattern fileName settings.InFilePatterns &&
        this.EmptyOrNotMatchesAnyPattern fileName settings.OutFilePatterns

    member this.IsMatchingFilePath (filePath: string) : bool =
        this.IsMatchingExtension (FileUtil.GetFilePathExtension filePath) &&
        this.IsMatchingFileName (Path.GetFileName filePath)

    member this.IsMatchingFileType (fileType : FileType) : bool =
        this.EmptyOrMatchesAnyFileType fileType settings.InFileTypes &&
        this.EmptyOrNotMatchesAnyFileType fileType settings.OutFileTypes

    member this.IsMatchingFileSize (fileSize : int64) : bool =
        (settings.MinSize <= 0 || fileSize >= settings.MinSize) &&
        (settings.MaxSize <= 0 || fileSize <= settings.MaxSize)

    member this.IsMatchingLastMod (lastMod : DateTime Option) : bool =
        (settings.MinLastMod.IsNone || lastMod.Value >= settings.MinLastMod.Value) &&
        (settings.MaxLastMod.IsNone || lastMod.Value <= settings.MaxLastMod.Value)

    member this.IsMatchingFileResult (fr : FileResult.t) : bool =
        this.IsMatchingFilePath(fr.FilePath) &&
        this.IsMatchingFileType(fr.Type) &&
        this.IsMatchingFileSize(fr.Size) &&
        this.IsMatchingLastMod(fr.LastMod)

    member this.FilterArchiveFilePathToFileResult (filePath: string) : FileResult.t Option =
        if not settings.IncludeArchives && not settings.ArchivesOnly then
            None
        elif not (this.IsMatchingArchiveFilePath filePath) then
            None
        else
            let fr = FileResult.Create filePath FileType.Archive 0L None
            Some fr

    member this.FilterRegularFilePathToFileResult (filePath: string) (fileType: FileType) : FileResult.t Option =
        if settings.ArchivesOnly then
            None
        elif not (this.IsMatchingFilePath filePath) || not (this.IsMatchingFileType fileType) then
            None
        else
            let fileInfo = FileInfo(filePath)
            let size = fileInfo.Length
            let lastMod = Some fileInfo.LastWriteTimeUtc
            if not (this.IsMatchingFileSize size) || not (this.IsMatchingLastMod lastMod) then
                None
            else
                let fr = FileResult.Create filePath fileType fileInfo.Length (Some fileInfo.LastWriteTimeUtc)
                Some fr

    member this.FilterFilePathToFileResult (filePath: string) : FileResult.t Option =
        if not (this.IsNullOrMatchingDirPath (Path.GetDirectoryName filePath)) ||
           not (this.IsMatchingFileNameByHidden (Path.GetFileName filePath)) then
            None
        else
            let fileType = (_fileTypes.GetFileTypeForFilePath filePath)
            if fileType = FileType.Archive then this.FilterArchiveFilePathToFileResult filePath
            else this.FilterRegularFilePathToFileResult filePath fileType

    member this.RecGetFileResults (dirPath : string) (minDepth : int) (maxDepth : int) (currentDepth : int) : FileResult.t list =
        if maxDepth > -1 && currentDepth > maxDepth then
            []
        else
            let dir = DirectoryInfo(dirPath)
            let symLinkFilter =
                if settings.FollowSymlinks then (fun _ -> true) else (fun f -> not (FileUtil.IsSymlink(f)))
            let fileResults =
                if minDepth < 0 || currentDepth >= minDepth then
                    dir.EnumerateFiles("*", _enumerationOptions)
                    |> Seq.map (fun f -> Path.Join(dirPath, f.Name))
                    |> Seq.filter symLinkFilter
                    |> Seq.choose this.FilterFilePathToFileResult
                    |> List.ofSeq
                else []
            let dirResults =
                if maxDepth < 0 || currentDepth < maxDepth then
                    dir.EnumerateDirectories()
                    |> Seq.map (fun d -> Path.Join(dirPath, d.Name))
                    |> Seq.filter symLinkFilter
                    |> Seq.filter this.IsTraversableDirPath
                    |> Seq.map (fun d -> this.RecGetFileResults d minDepth maxDepth (currentDepth + 1))
                    |> Seq.collect id
                    |> List.ofSeq
                else []
            List.concat [fileResults; dirResults]

    member this.GetFileResults (path: string) : Result<FileResult.t list, string> =
        let p =
            if Directory.Exists(path) || File.Exists(path)
            then path
            else FileUtil.ExpandPath(path)
        if not settings.FollowSymlinks && FileUtil.IsSymlink(p) then
            Error FindError.StartpathNotMatchFindSettings
        else
            if Directory.Exists(p) then
                // if MaxDepth is zero, we can skip since a directory cannot be a result
                if settings.MaxDepth <> 0 then
                    if (this.IsTraversableDirPath path) then
                        let maxDepth = if settings.Recursive then settings.MaxDepth else 1
                        Ok (this.RecGetFileResults p settings.MinDepth maxDepth 1)
                    else
                        Error FindError.StartpathNotMatchFindSettings
                else
                    Error FindError.StartpathNotMatchFindSettings
            elif File.Exists(p) then
                // if MinDepth > zero, we can skip since the file is at depth zero
                if settings.MinDepth <= 0 then
                    let fileResult = this.FilterFilePathToFileResult p
                    if fileResult.IsSome then
                        Ok [fileResult.Value]
                    else
                        Error FindError.StartpathNotMatchFindSettings
                else
                    Error FindError.StartpathNotMatchFindSettings
            else
                Error FindError.StartpathNotMatchFindSettings

    member this.Find () : Result<FileResult.t list, string> =
        this.SetEnumerationOptions()
        let results : Result<FileResult.t list, string> list = settings.Paths |> List.map this.GetFileResults
        if List.exists (fun (r : Result<FileResult.t list, string>) -> r.IsError) results then
            let err = results |> List.find _.IsError
            err
        else
            let fileResults = results |> List.map (Result.defaultValue []) |> List.collect id
            if List.length fileResults > 1 then
                let fileResultSorter = FileResultSorter(settings)
                Ok (fileResultSorter.Sort fileResults)
            else
                Ok fileResults

    member this.GetMatchingDirPaths (fileResults : FileResult.t list) : string list = 
        fileResults
        |> Seq.map (fun fr -> Path.GetDirectoryName(fr.FilePath))
        |> Seq.distinct
        |> List.ofSeq

    member this.PrintMatchingDirs (fileResults : FileResult.t list) (formatter : FileResultFormatter) : unit = 
        let dirs = this.GetMatchingDirPaths fileResults
        if dirs.Length > 0 then
            Logger.Log $"\nMatching directories (%d{dirs.Length}):"
            for d in dirs do
                printfn $"%s{formatter.FormatDirPath(d)}"
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
