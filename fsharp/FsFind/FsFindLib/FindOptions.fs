namespace FsFind

open System
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

    let argActionMap : Map<string, string -> FindSettings -> Unit> =
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
            ("maxdepth", (fun (s : string) (settings : FindSettings) -> settings.MaxDepth <- int s));
            ("maxlastmod", (fun (s : string) (settings : FindSettings) -> settings.MaxLastMod <- Some(DateTime.Parse(s))));
            ("maxsize", (fun (s : string) (settings : FindSettings) -> settings.MaxSize <- int s));
            ("mindepth", (fun (s : string) (settings : FindSettings) -> settings.MinDepth <- int s));
            ("minlastmod", (fun (s : string) (settings : FindSettings) -> settings.MinLastMod <- Some(DateTime.Parse(s))));
            ("minsize", (fun (s : string) (settings : FindSettings) -> settings.MinSize <- int s));
            ("path", (fun (s : string) (settings : FindSettings) -> settings.Paths <- settings.AddPath s settings.Paths));
            ("sort-by", (fun (s : string) (settings : FindSettings) -> settings.SortBy <- SortUtil.SortByFromName s));
        ] |> Map.ofList

    let flagActionMap : Map<string, bool -> FindSettings -> Unit> =
        [
            ("archivesonly", (fun (b : bool) (settings : FindSettings) -> settings.ArchivesOnly <- b));
            ("debug", (fun (b : bool) (settings : FindSettings) -> settings.Debug <- b));
            ("excludearchives", (fun (b : bool) (settings : FindSettings) -> settings.IncludeArchives <- not b));
            ("excludehidden", (fun (b : bool) (settings : FindSettings) -> settings.IncludeHidden <- not b));
            ("help", (fun (b : bool) (settings : FindSettings) -> settings.PrintUsage <- b));
            ("includearchives", (fun (b : bool) (settings : FindSettings) -> settings.IncludeArchives <- b));
            ("includehidden", (fun (b : bool) (settings : FindSettings) -> settings.IncludeHidden <- b));
            ("listdirs", (fun (b : bool) (settings : FindSettings) -> settings.ListDirs <- b));
            ("listfiles", (fun (b : bool) (settings : FindSettings) -> settings.ListFiles <- b));
            ("nolistfiles", (fun (b : bool) (settings : FindSettings) -> settings.ListFiles <- not b));
            ("norecursive", (fun (b : bool) (settings : FindSettings) -> settings.Recursive <- not b));
            ("recursive", (fun (b : bool) (settings : FindSettings) -> settings.Recursive <- b));
            ("sort-ascending", (fun (b : bool) (settings : FindSettings) -> settings.SortDescending <- not b));
            ("sort-caseinsensitive", (fun (b : bool) (settings : FindSettings) -> settings.SortCaseInsensitive <- b));
            ("sort-casesensitive", (fun (b : bool) (settings : FindSettings) -> settings.SortCaseInsensitive <- not b));
            ("sort-descending", (fun (b : bool) (settings : FindSettings) -> settings.SortDescending <- b));
            ("verbose", (fun (b : bool) (settings : FindSettings) -> settings.Verbose <- b));
            ("version", (fun (b : bool) (settings : FindSettings) -> settings.PrintVersion <- b));
        ] |> Map.ofList;

    type FindOptionsDictionary = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<System.Collections.Generic.Dictionary<string,string>>>

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

    let SettingsFromArgs (args : string[]) : FindSettings * string =
        let optionNameMap =
            let shortArgs = seq { for opt in options do if opt.ShortArg <> "" then yield (opt.ShortArg, opt.LongArg) }
            let longArgs =  seq { for opt in options do yield (opt.LongArg, opt.LongArg) }
            Seq.append shortArgs longArgs
            |> Map.ofSeq

        let argRegex = Regex("^(?:-{1,2})(?<opt>.*)$")

        let (|IsOption|_|) (arg:string) =            
            let m = argRegex.Match(arg)
            if m.Success then Some(m.Groups["opt"].Value) else None

        let rec recSettingsFromArgs (argList : string list) (settings : FindSettings) : FindSettings * string =
            match argList with
            | [] -> settings, ""
            | head :: tail ->
                match head with
                | IsOption opt ->
                    if optionNameMap.ContainsKey(opt) then
                        let long = optionNameMap[opt]
                        if argActionMap.ContainsKey(long) then
                            match tail with
                            | [] ->
                                settings, $"Missing value for option: %s{opt}"
                            | aHead :: aTail ->
                                argActionMap[long] aHead settings
                                recSettingsFromArgs aTail settings
                        elif flagActionMap.ContainsKey(long) then
                            flagActionMap[long] true settings
                            if long = "help" then
                                recSettingsFromArgs [] settings
                            else
                                recSettingsFromArgs tail settings
                        else
                            settings, $"Invalid option: %s{opt}"
                    else
                        settings, $"Invalid option: %s{opt}"
                | _ ->
                    settings.Paths <- settings.AddPath head settings.Paths
                    recSettingsFromArgs tail settings
        let settings = FindSettings()
        // default ListFiles to true since running as cli
        settings.ListFiles <- true
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
