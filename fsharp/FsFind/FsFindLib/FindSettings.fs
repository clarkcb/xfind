namespace FsFind

open System
open System.Text.RegularExpressions

type FindSettings() =
    let mutable _archivesOnly : bool = false
    let mutable _debug : bool = false

    member this.ArchivesOnly
        with get () = _archivesOnly
        and set value =
            _archivesOnly <- value
            if value then
                this.IncludeArchives <- value

    member this.Debug
        with get () = _debug
        and set value =
            _debug <- value
            this.Verbose <- value


    member val ExcludeHidden : bool = true with get, set
    member val InArchiveExtensions : string list = [] with get, set
    member val InArchiveFilePatterns : Regex list = [] with get, set
    member val InDirPatterns : Regex list = [] with get, set
    member val InExtensions : string list = [] with get, set
    member val InFilePatterns : Regex list = [] with get, set
    member val InFileTypes : FileType list = [] with get, set
    member val IncludeArchives : bool = false with get, set
    member val ListDirs : bool = false with get, set
    member val ListFiles : bool = false with get, set
    member val MaxLastMod : DateTime option = None with get, set
    member val MaxSize : int = 0 with get, set
    member val MinLastMod : DateTime option = None with get, set
    member val MinSize : int = 0 with get, set
    member val OutArchiveExtensions : string list = [] with get, set
    member val OutArchiveFilePatterns : Regex list = [] with get, set
    member val OutDirPatterns : Regex list = [] with get, set
    member val OutExtensions : string list = [] with get, set
    member val OutFilePatterns : Regex list = [] with get, set
    member val OutFileTypes : FileType list = [] with get, set
    member val Paths : string list = [] with get, set
    member val PrintUsage : bool = false with get, set
    member val PrintVersion : bool = false with get, set
    member val Recursive : bool = true with get, set
    member val SortBy : SortBy = SortBy.FilePath with get, set
    member val SortCaseInsensitive : bool = false with get, set
    member val SortDescending : bool = false with get, set
    member val Verbose : bool = false with get, set
    
    member this.AddExtensions (exts : string) (extList : string list) : string list =
        List.append extList (FileUtil.ExtensionsListFromString exts)

    member this.AddPath (path : string) (paths : string list) : string list =
        List.append paths [path]

    member this.AddPattern (pattern : string) (patternList : Regex list) : Regex list =
        List.append patternList [Regex(pattern, RegexOptions.Compiled)]

    member this.DateTimeOptionListToString (dt : DateTime option) : string =
        match dt with
        | Some(d) -> $"\"%s{d.ToString()}\""
        | None    -> "0"

    member this.FileTypesListToString (lst : FileType list) : string = 
        let rec recListToString (acc : string) (lst : FileType list) =
            match lst with
            | []     -> acc.Trim()
            | [a]    -> (recListToString (acc + " \"" + (FileTypes.ToName a) + "\"") [])
            | h :: t -> (recListToString (acc + " \"" + (FileTypes.ToName h) + "\";") t) in
        sprintf "[%s]" (recListToString "" lst)

    member this.FileTypesListFromString (fts : string) : FileType list =
        let nonWord = Regex(@"\W+")
        nonWord.Split(fts)
        |> Array.toList
        |> List.filter (fun (x : string) -> String.IsNullOrEmpty(x) = false)
        |> List.map (fun (x : string) -> FileTypes.FromName x)

    member this.AddFileTypes (fts : string) (ftList : FileType list) : FileType list =
        List.append ftList (this.FileTypesListFromString fts)

    member this.ToString =
        String.concat "" [
            "FindSettings(";
            $"ArchivesOnly: %b{this.ArchivesOnly}";
            $", Debug: %b{this.Debug}";
            $", ExcludeHidden: %b{this.ExcludeHidden}";
            $", InArchiveExtensions: %s{Common.ListToString(this.InArchiveExtensions)}";
            $", InArchiveFilePatterns: %s{Common.ListToString(this.InArchiveFilePatterns)}";
            $", InDirPatterns: %s{Common.ListToString(this.InDirPatterns)}";
            $", InExtensions: %s{Common.ListToString(this.InExtensions)}";
            $", InFilePatterns: %s{Common.ListToString(this.InFilePatterns)}";
            $", InFileTypes: %s{this.FileTypesListToString this.InFileTypes}";
            $", IncludeArchives: %b{this.IncludeArchives}";
            $", ListDirs: %b{this.ListDirs}";
            $", ListFiles: %b{this.ListFiles}";
            $", MaxLastMod: %s{this.DateTimeOptionListToString this.MaxLastMod}";
            $", MaxSize: %i{this.MaxSize}";
            $", MinLastMod: %s{this.DateTimeOptionListToString this.MinLastMod}";
            $", MinSize: %i{this.MinSize}";
            $", OutArchiveExtensions: %s{Common.ListToString(this.OutArchiveExtensions)}";
            $", OutArchiveFilePatterns: %s{Common.ListToString(this.OutArchiveFilePatterns)}";
            $", OutDirPatterns: %s{Common.ListToString(this.OutDirPatterns)}";
            $", OutExtensions: %s{Common.ListToString(this.OutExtensions)}";
            $", OutFilePatterns: %s{Common.ListToString(this.OutFilePatterns)}";
            $", OutFileTypes: %s{this.FileTypesListToString this.OutFileTypes}";
            $", Paths: %s{Common.ListToString(this.Paths)}";
            $", PrintUsage: %b{this.PrintUsage}";
            $", PrintVersion: %b{this.PrintVersion}";
            $", Recursive: %b{this.Recursive}";
            $", SortBy: %s{SortUtil.NameFromSortBy(this.SortBy)}";
            $", SortCaseInsensitive: %b{this.SortCaseInsensitive}";
            $", SortDescending: %b{this.SortDescending}";
            $", Verbose: %b{this.Verbose}";
            ")"
        ]
;;
