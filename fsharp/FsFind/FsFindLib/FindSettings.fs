namespace FsFind

open System.Text.RegularExpressions

module FindSettings =
    type t = {
        ArchivesOnly : bool;
        Colorize : bool;
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
        Colorize = true;
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
            sprintf "ArchivesOnly: %b" settings.ArchivesOnly;
            sprintf ", Colorize: %b" settings.Colorize;
            sprintf ", Debug: %b" settings.Debug;
            sprintf ", ExcludeHidden: %b" settings.ExcludeHidden;
            sprintf ", InArchiveExtensions: %s" (Common.list_to_string(settings.InArchiveExtensions));
            sprintf ", InArchiveFilePatterns: %s" (Common.list_to_string(settings.InArchiveFilePatterns));
            sprintf ", InDirPatterns: %s" (Common.list_to_string(settings.InDirPatterns));
            sprintf ", InExtensions: %s" (Common.list_to_string(settings.InExtensions));
            sprintf ", InFilePatterns: %s" (Common.list_to_string(settings.InFilePatterns));
            sprintf ", InFileTypes: %s" (FileTypesListToString settings.InFileTypes);
            sprintf ", IncludeArchives: %b" settings.IncludeArchives;
            sprintf ", ListDirs: %b" settings.ListDirs;
            sprintf ", ListFiles: %b" settings.ListFiles;
            sprintf ", OutArchiveExtensions: %s" (Common.list_to_string(settings.OutArchiveExtensions));
            sprintf ", OutArchiveFilePatterns: %s" (Common.list_to_string(settings.OutArchiveFilePatterns));
            sprintf ", OutDirPatterns: %s" (Common.list_to_string(settings.OutDirPatterns));
            sprintf ", OutExtensions: %s" (Common.list_to_string(settings.OutExtensions));
            sprintf ", OutFilePatterns: %s" (Common.list_to_string(settings.OutFilePatterns));
            sprintf ", OutFileTypes: %s" (FileTypesListToString settings.OutFileTypes);
            sprintf ", Paths: %s" (Common.list_to_string(settings.Paths));
            sprintf ", PrintUsage: %b" settings.PrintUsage;
            sprintf ", PrintVersion: %b" settings.PrintVersion;
            sprintf ", Recursive: %b" settings.Recursive;
            sprintf ", Verbose: %b" settings.Verbose;
            ")"
        ]
;;
