namespace FsFind

open System.Text.RegularExpressions

module FindSettings =
    type t = {
        ArchivesOnly : bool;
        Debug : bool;
        ExcludeHidden : bool;
        InArchiveExtensions : string list;
        InArchiveFilePatterns : Regex list;
        InDirPatterns : Regex list;
        InExtensions : string list;
        InFilePatterns : Regex list;
        InFileTypes : FileType list;
        IncludeArchives : bool;
        ListDirs : bool;
        ListFiles : bool;
        MaxLastMod : System.DateTime option;
        MaxSize : int;
        MinLastMod : System.DateTime option;
        MinSize : int;
        OutArchiveExtensions : string list;
        OutArchiveFilePatterns : Regex list;
        OutDirPatterns : Regex list;
        OutExtensions : string list;
        OutFilePatterns : Regex list;
        OutFileTypes : FileType list;
        Paths : string list;
        PrintUsage : bool;
        PrintVersion : bool;
        Recursive : bool;
        SortBy : SortBy;
        SortCaseInsensitive : bool;
        SortDescending : bool;
        Verbose : bool
    }

    let DefaultSettings = {
        ArchivesOnly = false;
        Debug = false;
        ExcludeHidden = true;
        InArchiveExtensions = [];
        InArchiveFilePatterns = [];
        InDirPatterns = [];
        InExtensions = [];
        InFilePatterns = [];
        InFileTypes = [];
        IncludeArchives = false;
        ListDirs = false;
        ListFiles = false;
        MaxLastMod = None;
        MaxSize = 0;
        MinLastMod = None;
        MinSize = 0;
        OutArchiveExtensions = [];
        OutArchiveFilePatterns = [];
        OutDirPatterns = [];
        OutExtensions = [];
        OutFilePatterns = [];
        OutFileTypes = [];
        Paths = [];
        PrintUsage = false;
        PrintVersion = false;
        Recursive = true;
        SortBy = SortBy.FilePath;
        SortCaseInsensitive = false;
        SortDescending = false;
        Verbose = false
    }

    let AddExtensions (exts : string) (extList : string list) : string list =
        List.append extList (FileUtil.ExtensionsListFromString exts)

    let AddPath (path : string) (settings : t) : t =
        { settings with Paths=(List.append settings.Paths [path]) }

    let AddPattern (pattern : string) (patternList : Regex list) : Regex list =
        List.append patternList [Regex(pattern, RegexOptions.Compiled)]

    let SetArchivesOnly (archivesOnly : bool) (settings : t) : t =
        match archivesOnly with
        | true -> { settings with ArchivesOnly=true; IncludeArchives=true }
        | _ -> { settings with ArchivesOnly=false }

    let SetDebug (debug : bool) (settings : t) : t =
        match debug with
        | true -> { settings with Debug=true; Verbose=true }
        | _ -> { settings with Debug=false }

    let DateTimeOptionListToString (dt : System.DateTime option) : string =
        match dt with
        | Some(d) -> $"\"%s{d.ToString()}\""
        | None    -> "0"

    let FileTypesListToString (lst : FileType list) : string = 
        let rec recListToString (acc : string) (lst : FileType list) =
            match lst with
            | []     -> acc.Trim()
            | [a]    -> (recListToString (acc + " \"" + (FileTypes.ToName a) + "\"") [])
            | h :: t -> (recListToString (acc + " \"" + (FileTypes.ToName h) + "\";") t) in
        sprintf "[%s]" (recListToString "" lst)

    let ToString settings =
        String.concat "" [
            "FindSettings(";
            $"ArchivesOnly: %b{settings.ArchivesOnly}";
            $", Debug: %b{settings.Debug}";
            $", ExcludeHidden: %b{settings.ExcludeHidden}";
            $", InArchiveExtensions: %s{Common.list_to_string(settings.InArchiveExtensions)}";
            $", InArchiveFilePatterns: %s{Common.list_to_string(settings.InArchiveFilePatterns)}";
            $", InDirPatterns: %s{Common.list_to_string(settings.InDirPatterns)}";
            $", InExtensions: %s{Common.list_to_string(settings.InExtensions)}";
            $", InFilePatterns: %s{Common.list_to_string(settings.InFilePatterns)}";
            $", InFileTypes: %s{FileTypesListToString settings.InFileTypes}";
            $", IncludeArchives: %b{settings.IncludeArchives}";
            $", ListDirs: %b{settings.ListDirs}";
            $", ListFiles: %b{settings.ListFiles}";
            $", MaxLastMod: %s{DateTimeOptionListToString settings.MaxLastMod}";
            $", MaxSize: %i{settings.MaxSize}";
            $", MinLastMod: %s{DateTimeOptionListToString settings.MinLastMod}";
            $", MinSize: %i{settings.MinSize}";
            $", OutArchiveExtensions: %s{Common.list_to_string(settings.OutArchiveExtensions)}";
            $", OutArchiveFilePatterns: %s{Common.list_to_string(settings.OutArchiveFilePatterns)}";
            $", OutDirPatterns: %s{Common.list_to_string(settings.OutDirPatterns)}";
            $", OutExtensions: %s{Common.list_to_string(settings.OutExtensions)}";
            $", OutFilePatterns: %s{Common.list_to_string(settings.OutFilePatterns)}";
            $", OutFileTypes: %s{FileTypesListToString settings.OutFileTypes}";
            $", Paths: %s{Common.list_to_string(settings.Paths)}";
            $", PrintUsage: %b{settings.PrintUsage}";
            $", PrintVersion: %b{settings.PrintVersion}";
            $", Recursive: %b{settings.Recursive}";
            $", SortBy: %s{SortUtil.NameFromSortBy(settings.SortBy)}";
            $", SortCaseInsensitive: %b{settings.SortCaseInsensitive}";
            $", SortDescending: %b{settings.SortDescending}";
            $", Verbose: %b{settings.Verbose}";
            ")"
        ]
;;
