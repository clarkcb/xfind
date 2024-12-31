namespace FsFind

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Json
open System.Text.RegularExpressions

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

    let boolActionMap : Map<string, bool -> FindSettings -> Unit> =
        [
            ("archivesonly", (fun (b : bool) (settings : FindSettings) -> settings.ArchivesOnly <- b));
            ("debug", (fun (b : bool) (settings : FindSettings) -> settings.Debug <- b));
            ("excludearchives", (fun (b : bool) (settings : FindSettings) -> settings.IncludeArchives <- not b));
            ("excludehidden", (fun (b : bool) (settings : FindSettings) -> settings.IncludeHidden <- not b));
            ("followsymlinks", (fun (b : bool) (settings : FindSettings) -> settings.FollowSymlinks <- b));
            ("help", (fun (b : bool) (settings : FindSettings) -> settings.PrintUsage <- b));
            ("includearchives", (fun (b : bool) (settings : FindSettings) -> settings.IncludeArchives <- b));
            ("includehidden", (fun (b : bool) (settings : FindSettings) -> settings.IncludeHidden <- b));
            ("nofollowsymlinks", (fun (b : bool) (settings : FindSettings) -> settings.FollowSymlinks <- not b));
            ("noprintdirs", (fun (b : bool) (settings : FindSettings) -> settings.PrintDirs <- not b));
            ("noprintfiles", (fun (b : bool) (settings : FindSettings) -> settings.PrintFiles <- not b));
            ("norecursive", (fun (b : bool) (settings : FindSettings) -> settings.Recursive <- not b));
            ("printdirs", (fun (b : bool) (settings : FindSettings) -> settings.PrintDirs <- b));
            ("printfiles", (fun (b : bool) (settings : FindSettings) -> settings.PrintFiles <- b));
            ("recursive", (fun (b : bool) (settings : FindSettings) -> settings.Recursive <- b));
            ("sort-ascending", (fun (b : bool) (settings : FindSettings) -> settings.SortDescending <- not b));
            ("sort-caseinsensitive", (fun (b : bool) (settings : FindSettings) -> settings.SortCaseInsensitive <- b));
            ("sort-casesensitive", (fun (b : bool) (settings : FindSettings) -> settings.SortCaseInsensitive <- not b));
            ("sort-descending", (fun (b : bool) (settings : FindSettings) -> settings.SortDescending <- b));
            ("verbose", (fun (b : bool) (settings : FindSettings) -> settings.Verbose <- b));
            ("version", (fun (b : bool) (settings : FindSettings) -> settings.PrintVersion <- b));
        ] |> Map.ofList;

    let stringActionMap : Map<string, string -> FindSettings -> Unit> =
        [
            ("in-archiveext", (fun (s : string) (settings : FindSettings) -> settings.InArchiveExtensions <- settings.AddExtensions s settings.InArchiveExtensions));
            ("in-archivefilepattern", (fun (s : string) (settings : FindSettings) -> settings.InArchiveFilePatterns <- settings.AddPattern s settings.InArchiveFilePatterns));
            ("in-dirpattern", (fun (s : string) (settings : FindSettings) -> settings.InDirPatterns <- settings.AddPattern s settings.InDirPatterns));
            ("in-ext", (fun (s : string) (settings : FindSettings) -> settings.InExtensions <- settings.AddExtensions s settings.InExtensions));
            ("in-filepattern", (fun (s : string) (settings : FindSettings) -> settings.InFilePatterns <- settings.AddPattern s settings.InFilePatterns));
            ("in-filetype", (fun (s : string) (settings : FindSettings) -> settings.InFileTypes <- settings.AddFileTypes s settings.InFileTypes));
            ("out-archiveext", (fun (s : string) (settings : FindSettings) -> settings.OutArchiveExtensions <- settings.AddExtensions s settings.OutArchiveExtensions));
            ("out-archivefilepattern", (fun (s : string) (settings : FindSettings) -> settings.OutArchiveFilePatterns <- settings.AddPattern s settings.OutArchiveFilePatterns));
            ("out-dirpattern", (fun (s : string) (settings : FindSettings) -> settings.OutDirPatterns <- settings.AddPattern s settings.OutDirPatterns));
            ("out-ext", (fun (s : string) (settings : FindSettings) -> settings.OutExtensions <- settings.AddExtensions s settings.OutExtensions));
            ("out-filepattern", (fun (s : string) (settings : FindSettings) -> settings.OutFilePatterns <- settings.AddPattern s settings.OutFilePatterns));
            ("out-filetype", (fun (s : string) (settings : FindSettings) -> settings.OutFileTypes <- settings.AddFileTypes s settings.OutFileTypes));
            ("maxlastmod", (fun (s : string) (settings : FindSettings) -> settings.MaxLastMod <- Some(DateTime.Parse(s))));
            ("minlastmod", (fun (s : string) (settings : FindSettings) -> settings.MinLastMod <- Some(DateTime.Parse(s))));
            ("path", (fun (s : string) (settings : FindSettings) -> settings.Paths <- settings.AddPath s settings.Paths));
            ("sort-by", (fun (s : string) (settings : FindSettings) -> settings.SortBy <- SortUtil.SortByFromName s));
        ] |> Map.ofList

    let intActionMap : Map<string, int -> FindSettings -> Unit> =
        [
            ("maxdepth", (fun (i : int) (settings : FindSettings) -> settings.MaxDepth <- i));
            ("maxsize", (fun (i : int) (settings : FindSettings) -> settings.MaxSize <- i));
            ("mindepth", (fun (i : int) (settings : FindSettings) -> settings.MinDepth <- i));
            ("minsize", (fun (i : int) (settings : FindSettings) -> settings.MinSize <- i));
        ] |> Map.ofList

    type FindOptionsDictionary = Dictionary<string, List<Dictionary<string,string>>>

    let OptionsFromJson (jsonString : string) : FindOption list =
        let findOptionsDict = JsonSerializer.Deserialize<FindOptionsDictionary>(jsonString)
        let optionDicts = findOptionsDict["findoptions"]
        [ for optionDict in optionDicts do
            let longArg = optionDict["long"]
            let shortArg = if optionDict.ContainsKey("short") then optionDict["short"] else ""
            let desc = optionDict["desc"]
            yield { ShortArg=shortArg; LongArg=longArg; Description=desc } ]

    let _findOptionsResource = EmbeddedResource.GetResourceFileContents("FsFindLib.Resources.findoptions.json");
    let options = OptionsFromJson(_findOptionsResource)

    let rec ApplySetting (arg : string) (elem : JsonElement) (settings : FindSettings) : Result<FindSettings, string> =
        match (boolActionMap.ContainsKey(arg), stringActionMap.ContainsKey(arg), intActionMap.ContainsKey(arg)) with
        | true, false, false ->
            if elem.ValueKind = JsonValueKind.False then
                boolActionMap[arg] false settings
                Ok settings
            else
                if elem.ValueKind = JsonValueKind.True then
                    boolActionMap[arg] true settings
                    Ok settings
                else
                    Error $"Invalid value for option: {arg}"
        | false, true, false ->
            if elem.ValueKind = JsonValueKind.String then
                let s = elem.GetString()
                if s = null then
                    Error $"Invalid value for option: {arg}"
                else
                    stringActionMap[arg] s settings
                    Ok settings
            else
                if elem.ValueKind = JsonValueKind.Array then
                    let rec recApplySettings (elems : JsonElement list) (settings : FindSettings) : Result<FindSettings, string> =
                        match elems with
                        | [] -> Ok settings
                        | elem :: tail ->
                            match ApplySetting arg elem settings with
                            | Ok nextSettings -> recApplySettings tail nextSettings
                            | Error e -> Error e
                    let elems = elem.EnumerateArray() |> List.ofSeq
                    recApplySettings elems settings
                else
                    Error $"Invalid value for option: {arg}"
        | false, false, true ->
            if elem.ValueKind = JsonValueKind.Number then
                intActionMap[arg] (elem.GetInt32()) settings
                Ok settings
            else
                Error $"Invalid value for option: {arg}"
        | _ ->
            Error $"Invalid option: {arg}"

    let UpdateSettingsFromJson (jsonString : string) (settings : FindSettings) : Result<FindSettings, string> =
        match JsonSerializer.Deserialize<Dictionary<string, JsonElement>>(jsonString) with
        | null -> Error "Unable to parse json"
        | settingsDict ->
            let rec recSettingsFromArgs (argList : string list) (settings : FindSettings) : Result<FindSettings, string> =
                match argList with
                | [] -> Ok settings
                | arg :: tail ->
                    match ApplySetting arg settingsDict[arg] settings with
                    | Ok settings -> recSettingsFromArgs tail settings
                    | Error e -> Error e
            // keys are sorted so that output is consistent across all versions
            let argList = settingsDict.Keys |> List.ofSeq |> List.sort
            recSettingsFromArgs argList settings

    let UpdateSettingsFromFile (filePath : string) (settings : FindSettings) : Result<FindSettings, string> =
        let expandedPath = FileUtil.ExpandPath(filePath)
        let fileInfo = new FileInfo(expandedPath)
        if fileInfo.Exists then
            if fileInfo.Extension.Equals(".json") then
                let contents = FileUtil.GetFileContents expandedPath Encoding.Default
                UpdateSettingsFromJson contents settings
            else
                Error $"Invalid settings file (must be JSON): {filePath}"
        else
            Error $"Settings file not found: {filePath}"

    let SettingsFromJson (jsonString : string) : Result<FindSettings, string> =
        let settings = FindSettings()
        UpdateSettingsFromJson jsonString settings

    let SettingsFromFile (filePath : string) : Result<FindSettings, string> =
        let settings = FindSettings()
        UpdateSettingsFromFile filePath settings

    let GetOptionNameMap : Map<string, string> =
        let shortArgs = seq { for opt in options do if opt.ShortArg <> "" then yield (opt.ShortArg, opt.LongArg) }
        let longArgs =  seq { for opt in options do yield (opt.LongArg, opt.LongArg) }
        Seq.append shortArgs longArgs
        |> Map.ofSeq
        
    let SettingsFromArgs (args : string[]) : Result<FindSettings, string> =
        let optionNameMap = GetOptionNameMap

        let argRegex = Regex("^(?:-{1,2})(?<opt>.*)$")

        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups["opt"].Value) else None

        let rec recSettingsFromArgs (argList : string list) (settings : FindSettings) : Result<FindSettings, string> =
            match argList with
            | [] -> Ok settings
            | head :: tail ->
                match head with
                | IsOption opt ->
                    if optionNameMap.ContainsKey(opt) then
                        let longArg = optionNameMap[opt]
                        if boolActionMap.ContainsKey(longArg) then
                            boolActionMap[longArg] true settings
                            if longArg = "help" then
                                recSettingsFromArgs [] settings
                            else
                                recSettingsFromArgs tail settings
                        elif stringActionMap.ContainsKey(longArg) || intActionMap.ContainsKey(longArg) || longArg.Equals("settings-file") then
                            match tail with
                            | [] ->
                                Error $"Missing value for option: %s{opt}"
                            | aHead :: aTail ->
                                if stringActionMap.ContainsKey(longArg) then
                                    stringActionMap[longArg] aHead settings
                                    recSettingsFromArgs aTail settings
                                else
                                    if intActionMap.ContainsKey(longArg) then
                                        intActionMap[longArg] (int aHead) settings
                                        recSettingsFromArgs aTail settings
                                    else
                                        match UpdateSettingsFromFile aHead settings with
                                        | Ok fileSettings -> recSettingsFromArgs aTail fileSettings
                                        | Error e -> Error e
                        else
                            Error $"Invalid option: %s{opt}"
                    else
                        Error $"Invalid option: %s{opt}"
                | _ ->
                    settings.Paths <- settings.AddPath head settings.Paths
                    recSettingsFromArgs tail settings
        let settings = FindSettings()
        // default PrintFiles to true since running as cli
        settings.PrintFiles <- true
        recSettingsFromArgs (Array.toList args) settings

    let GetUsageString () : string =
        let sortedOptions = options |> List.sortWith SortOption
        let optStringMap =
            [ for opt in sortedOptions do
                let shortString : string = 
                    if opt.ShortArg <> "" then "-" + opt.ShortArg + ","
                    else ""
                let longString : string = "--" + opt.LongArg
                yield (opt.LongArg, shortString + longString) ]
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
                yield String.Format(format, optStringMap[o.LongArg], optDescMap[o.LongArg])]

        let usageString = 
            usageStrings
            |> List.append ["\nUsage:"; " fsfind [options] <path> [<path> ...]\n"; "Options:"] 
            |> String.concat "\n"
        usageString

    let Usage (exitCode : int) : unit =
        let usageString = GetUsageString()
        printfn $"%s{usageString}\n"
        Environment.Exit(exitCode)
