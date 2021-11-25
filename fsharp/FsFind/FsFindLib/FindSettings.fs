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
        OutArchiveExtensions : string list;
        OutArchiveFilePatterns : Regex list;
        OutDirPatterns : Regex list;
        OutExtensions : string list;
        OutFilePatterns : Regex list;
        OutFileTypes : FileType list;
        PrintUsage : bool;
        PrintVersion : bool;
        Recursive : bool;
        Paths : string list;
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
        OutArchiveExtensions = [];
        OutArchiveFilePatterns = [];
        OutDirPatterns = [];
        OutExtensions = [];
        OutFilePatterns = [];
        OutFileTypes = [];
        PrintUsage = false;
        PrintVersion = false;
        Recursive = true;
        Paths = [];
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
            $", Verbose: %b{settings.Verbose}";
            ")"
        ]
;;
