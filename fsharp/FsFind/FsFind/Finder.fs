namespace FsFind

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

type Finder (settings : FindSettings.t) =
    let _fileTypes = FileTypes()

    // member methods
    member this.ValidateSettings () : string list =
        [
            (if List.isEmpty settings.Paths then (Some "Startpath not defined") else None);
            // (if Directory.Exists(settings.StartPath) || File.Exists(settings.StartPath) then None else (Some "Startpath not found"));
        ]
        |> List.filter (fun e -> e.IsSome)
        |> List.map (fun e -> e.Value)

    member this.MatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        Seq.exists (fun p -> (p:Regex).Match(s).Success) patterns

    member this.AnyMatchesAnyPattern (slist : string seq) (patterns : Regex list) : bool =
        Seq.exists (fun s -> this.MatchesAnyPattern s patterns) slist

    member this.IsFindDir (d : DirectoryInfo) : bool =
        let elems = d.FullName.Split('/', '\\') |> Seq.filter (fun s -> not (String.IsNullOrEmpty s))
        (not settings.ExcludeHidden ||
         not (Seq.exists (fun e -> FileUtil.IsHidden e) elems)) &&
        (Seq.isEmpty settings.InDirPatterns ||
         this.AnyMatchesAnyPattern elems settings.InDirPatterns) &&
        (Seq.isEmpty settings.OutDirPatterns ||
         not (this.AnyMatchesAnyPattern elems settings.OutDirPatterns))

    member this.IsFindFile (f : FindFile.t) : bool =
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

    member this.IsArchiveFindFile (f : FindFile.t) : bool =
        (Seq.isEmpty settings.InArchiveExtensions ||
         Seq.exists (fun x -> x = f.File.Extension) settings.InArchiveExtensions) &&
        (Seq.isEmpty settings.OutArchiveExtensions ||
         not (Seq.exists (fun x -> x = f.File.Extension) settings.OutArchiveExtensions)) &&
        (Seq.isEmpty settings.InArchiveFilePatterns ||
         Seq.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.InArchiveFilePatterns) &&
        (Seq.isEmpty settings.OutArchiveFilePatterns ||
         not (Seq.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.OutArchiveFilePatterns))

    member this.FilterFile (f: FindFile.t) : bool = 
        if FileUtil.IsHiddenFile f.File && settings.ExcludeHidden then
            false
        else if f.FileType = FileType.Archive then
            settings.IncludeArchives && this.IsArchiveFindFile f
        else
            not settings.ArchivesOnly && this.IsFindFile f

    member this.GetFindFiles (path : string) : FindFile.t list =
        let expandedPath = FileUtil.ExpandPath path
        if Directory.Exists(expandedPath) then
            let findOption =
                if settings.Recursive then SearchOption.AllDirectories
                else SearchOption.TopDirectoryOnly
            let dir = DirectoryInfo(expandedPath)
            dir.EnumerateFiles("*", findOption)
            |> Seq.filter (fun f -> this.IsFindDir(f.Directory))
            |> Seq.map (fun f -> FindFile.Create f (_fileTypes.GetFileType f))
            |> Seq.filter (fun sf -> this.FilterFile sf)
            |> List.ofSeq
        else
            let fileInfo = FileInfo(expandedPath)
            [FindFile.Create fileInfo (_fileTypes.GetFileType fileInfo)]

    member this.Find () : FindFile.t list =
        settings.Paths
        |> List.collect (fun p -> this.GetFindFiles p)

    member this.GetMatchingDirs (findFiles : FindFile.t list) : DirectoryInfo list = 
        findFiles
        |> Seq.map (fun f -> f.File.Directory)
        |> Seq.distinctBy (fun d -> d.FullName)
        |> Seq.sortBy (fun d -> d.FullName)
        |> List.ofSeq

    member this.PrintMatchingDirs (findFiles : FindFile.t list) : unit = 
        let dirs = this.GetMatchingDirs findFiles
        if dirs.Length > 0 then
            Common.Log (sprintf "\nMatching directories (%d):" dirs.Length)
            for d in dirs do
                printfn "%s" d.FullName
        else
            Common.Log "\nMatching directories: 0"


    member this.GetMatchingFiles (findFiles : FindFile.t list) : FileInfo list = 
        findFiles
        |> Seq.map (fun f -> f.File)
        |> Seq.distinctBy (fun f -> f.FullName)
        |> Seq.sortBy (fun f -> f.FullName)
        |> List.ofSeq

    member this.PrintMatchingFiles (findFiles : FindFile.t list) : unit = 
        let files = this.GetMatchingFiles findFiles
        if files.Length > 0 then
            Common.Log (sprintf "\nMatching files (%d):" files.Length)
            for f in files do
                printfn "%s" f.FullName
        else
            Common.Log "\nMatching files: 0"

;;
