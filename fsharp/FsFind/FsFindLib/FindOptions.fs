namespace FsFind

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Xml.Linq

module FindOptions =   

    type FindOption = {
        ShortArg : string;
        LongArg : string;
        Description : string
    }

    let SortOption (o1 : FindOption) (o2 : FindOption) : int =
        let os1 = if o1.ShortArg <> "" then o1.ShortArg + "@" + o1.LongArg else o1.LongArg
        let os2 = if o2.ShortArg <> "" then o2.ShortArg + "@" + o2.LongArg else o2.LongArg
        String.Compare(os1, os2, StringComparison.OrdinalIgnoreCase)

    let AddExtensions (exts : string) (extList : string list) : string list =
        List.append extList (FileUtil.ExtensionsListFromString exts)

    let AddPattern (pattern : string) (patternList : Regex list) : Regex list =
        List.append patternList [Regex(pattern)]

    let FileTypesListFromString (fts : string) : FileType list =
        let nonWord = Regex(@"\W+")
        nonWord.Split(fts)
        |> Array.toList
        |> List.filter (fun (x : string) -> String.IsNullOrEmpty(x) = false)
        |> List.map (fun (x : string) -> FileTypes.FromName x)

    let AddFileTypes (fts : string) (ftList : FileType list) : FileType list =
        List.append ftList (FileTypesListFromString fts)

    let argActionMap : Map<string, string -> FindSettings.t -> FindSettings.t> =
        [
            ("in-archiveext", (fun (s : string) (settings : FindSettings.t) -> { settings with InArchiveExtensions = AddExtensions s settings.InArchiveExtensions }));
            ("in-archivefilepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with InArchiveFilePatterns = AddPattern s settings.InArchiveFilePatterns }));
            ("in-dirpattern", (fun (s : string) (settings : FindSettings.t) -> { settings with InDirPatterns = AddPattern s settings.InDirPatterns }));
            ("in-ext", (fun (s : string) (settings : FindSettings.t) -> { settings with InExtensions = AddExtensions s settings.InExtensions }));
            ("in-filepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with InFilePatterns = AddPattern s settings.InFilePatterns }));
            ("in-filetype", (fun (s : string) (settings : FindSettings.t) -> { settings with InFileTypes = AddFileTypes s settings.InFileTypes }));
            ("out-archiveext", (fun (s : string) (settings : FindSettings.t) -> { settings with OutArchiveExtensions = AddExtensions s settings.OutArchiveExtensions }));
            ("out-archivefilepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with OutArchiveFilePatterns = AddPattern s settings.OutArchiveFilePatterns }));
            ("out-dirpattern", (fun (s : string) (settings : FindSettings.t) -> { settings with OutDirPatterns = AddPattern s settings.OutDirPatterns }));
            ("out-ext", (fun (s : string) (settings : FindSettings.t) -> { settings with OutExtensions = AddExtensions s settings.OutExtensions }));
            ("out-filepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with OutFilePatterns = AddPattern s settings.OutFilePatterns }));
            ("out-filetype", (fun (s : string) (settings : FindSettings.t) -> { settings with OutFileTypes = AddFileTypes s settings.OutFileTypes }));
            ("maxlastmod", (fun (s : string) (settings : FindSettings.t) -> { settings with MaxLastMod = Some(DateTime.Parse(s)) }));
            ("maxsize", (fun (s : string) (settings : FindSettings.t) -> { settings with MaxSize = int s }));
            ("minlastmod", (fun (s : string) (settings : FindSettings.t) -> { settings with MinLastMod = Some(DateTime.Parse(s)) }));
            ("minsize", (fun (s : string) (settings : FindSettings.t) -> { settings with MinSize = int s }));
            ("path", (fun (s : string) (settings : FindSettings.t) -> FindSettings.AddPath s settings));
            ("sort-by", (fun (s : string) (settings : FindSettings.t) -> { settings with SortBy = SortUtil.SortByFromName s }));
        ] |> Map.ofList

    let flagActionMap : Map<string, bool -> FindSettings.t -> FindSettings.t> =
        [
            ("archivesonly", (fun (b : bool) (settings : FindSettings.t) -> FindSettings.SetArchivesOnly b settings));
            ("debug", (fun (b : bool) (settings : FindSettings.t) -> FindSettings.SetDebug b settings));
            ("excludearchives", (fun (b : bool) (settings : FindSettings.t) -> { settings with IncludeArchives = not b }));
            ("excludehidden", (fun (b : bool) (settings : FindSettings.t) -> { settings with ExcludeHidden = b }));
            ("help", (fun (b : bool) (settings : FindSettings.t) -> { settings with PrintUsage = b }));
            ("includearchives", (fun (b : bool) (settings : FindSettings.t) -> { settings with IncludeArchives = b }));
            ("includehidden", (fun (b : bool) (settings : FindSettings.t) -> { settings with ExcludeHidden = not b }));
            ("listdirs", (fun (b : bool) (settings : FindSettings.t) -> { settings with ListDirs = b }));
            ("listfiles", (fun (b : bool) (settings : FindSettings.t) -> { settings with ListFiles = b }));
            ("norecursive", (fun (b : bool) (settings : FindSettings.t) -> { settings with Recursive = not b }));
            ("recursive", (fun (b : bool) (settings : FindSettings.t) -> { settings with Recursive = b }));
            ("sort-ascending", (fun (b : bool) (settings : FindSettings.t) -> { settings with SortDescending = not b }));
            ("sort-caseinsensitive", (fun (b : bool) (settings : FindSettings.t) -> { settings with SortCaseInsensitive = b }));
            ("sort-casesensitive", (fun (b : bool) (settings : FindSettings.t) -> { settings with SortCaseInsensitive = not b }));
            ("sort-descending", (fun (b : bool) (settings : FindSettings.t) -> { settings with SortDescending = b }));
            ("verbose", (fun (b : bool) (settings : FindSettings.t) -> { settings with Verbose = b }));
            ("version", (fun (b : bool) (settings : FindSettings.t) -> { settings with PrintVersion = b }));
        ] |> Map.ofList;

    type FindOptionsDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string,string>>>

    let OptionsFromJson (jsonString : string) : FindOption list =
        let findOptionsDict = JsonSerializer.Deserialize<FindOptionsDictionary>(jsonString)
        let optionDicts = findOptionsDict.["findoptions"]
        [ for optionDict in optionDicts do
            let longArg = optionDict.["long"]
            let shortArg = if optionDict.ContainsKey("short") then optionDict.["short"] else ""
            let desc = optionDict.["desc"]
            yield { ShortArg=shortArg; LongArg=longArg; Description=desc } ]

    let _findOptionsResource = EmbeddedResource.GetResourceFileContents("FsFindLib.Resources.findoptions.json");
    let options = OptionsFromJson(_findOptionsResource)

    let SettingsFromArgs (args : string[]) : FindSettings.t * string =
        let optionNameMap =
            let shortargs = seq { for opt in options do if opt.ShortArg <> "" then yield (opt.ShortArg, opt.LongArg) }
            let longargs =  seq { for opt in options do yield (opt.LongArg, opt.LongArg) }
            Seq.append shortargs longargs
            |> Map.ofSeq

        let argRegex = Regex("^(?:-{1,2})(?<opt>.*)$")

        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups.["opt"].Value) else None

        let rec recSettingsFromArgs (argList : string list) (settings : FindSettings.t) : FindSettings.t * string =
            match argList with
            | [] -> settings, ""
            | head :: tail ->
                match head with
                | IsOption opt ->
                    if optionNameMap.ContainsKey(opt) then
                        let long = optionNameMap.[opt]
                        if argActionMap.ContainsKey(long) then
                            match tail with
                            | [] ->
                                settings, $"Missing value for option: %s{opt}"
                            | aHead :: aTail -> 
                                recSettingsFromArgs aTail (argActionMap.[long] aHead settings)
                        elif flagActionMap.ContainsKey(long) then
                            if long = "help" then
                                recSettingsFromArgs [] (flagActionMap.[long] true settings)
                            else
                                recSettingsFromArgs tail (flagActionMap.[long] true settings)
                        else
                            settings, $"Invalid option: %s{opt}"
                    else
                        settings, $"Invalid option: %s{opt}"
                | _ -> recSettingsFromArgs tail { settings with Paths = List.append settings.Paths [head] }
        // default ListFiles to true since running as cli
        recSettingsFromArgs (Array.toList args) { FindSettings.DefaultSettings with ListFiles = true }

    let GetUsageString () : string =
        let sortedOptions = options |> List.sortWith SortOption
        let optStringMap =
            [ for opt in sortedOptions do
                let shortstring : string = 
                    if opt.ShortArg <> "" then "-" + opt.ShortArg + ","
                    else ""
                let longstring : string = "--" + opt.LongArg
                yield (opt.LongArg, shortstring + longstring) ]
            |> Map.ofList

        let optDescMap =
            [ for opt in sortedOptions do
                yield (opt.LongArg, opt.Description) ]
            |> Map.ofList

        let optStrings : string list =
            optStringMap
            |> Map.toList
            |> List.map snd

        let longest = 
            optStrings
            |> Seq.map (fun o -> o.Length)
            |> Seq.max

        let format = " {0,-" + longest.ToString() + "}  {1}"

        let usageStrings =
            [for o in sortedOptions do
                yield String.Format(format, optStringMap.[o.LongArg], optDescMap.[o.LongArg])]

        let usageString = 
            usageStrings
            |> List.append ["\nUsage:"; " fsfind [options] <path> [<path> ...]\n"; "Options:"] 
            |> String.concat "\n"
        usageString

    let Usage (exitCode : int) : unit =
        let usageString = GetUsageString()
        printfn $"%s{usageString}\n"
        Environment.Exit(exitCode)
