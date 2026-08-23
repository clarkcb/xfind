namespace FsFindLib

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open FsFindLib

type FindOption = Option

type FindOptionsDictionary = Dictionary<string, List<Dictionary<string,string>>>

type FindOptions(config : FindConfig) =

    static let boolActionMap : Map<string, bool -> FindSettings -> Unit> =
        [
            ("archivesonly", (fun (b : bool) (settings : FindSettings) -> settings.ArchivesOnly <- b));
            ("colorize", (fun (b : bool) (settings : FindSettings) -> settings.Colorize <- b));
            ("debug", (fun (b : bool) (settings : FindSettings) -> settings.Debug <- b));
            ("defaultfiles", (fun (b : bool) (settings : FindSettings) -> settings.DefaultFiles <- b));
            ("excludearchives", (fun (b : bool) (settings : FindSettings) -> settings.IncludeArchives <- not b));
            ("excludehidden", (fun (b : bool) (settings : FindSettings) -> settings.IncludeHidden <- not b));
            ("followsymlinks", (fun (b : bool) (settings : FindSettings) -> settings.FollowSymlinks <- b));
            ("help", (fun (b : bool) (settings : FindSettings) -> settings.PrintUsage <- b));
            ("includearchives", (fun (b : bool) (settings : FindSettings) -> settings.IncludeArchives <- b));
            ("includehidden", (fun (b : bool) (settings : FindSettings) -> settings.IncludeHidden <- b));
            ("nocolorize", (fun (b : bool) (settings : FindSettings) -> settings.Colorize <- not b));
            ("nodefaultfiles", (fun (b : bool) (settings : FindSettings) -> settings.DefaultFiles <- not b));
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

    static let stringActionMap : Map<string, string -> FindSettings -> Unit> =
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

    static let intActionMap : Map<string, int -> FindSettings -> Unit> =
        [
            ("maxdepth", (fun (i : int) (settings : FindSettings) -> settings.MaxDepth <- i));
            ("mindepth", (fun (i : int) (settings : FindSettings) -> settings.MinDepth <- i));
        ] |> Map.ofList

    static let longActionMap : Map<string, int64 -> FindSettings -> Unit> =
        [
            ("maxsize", (fun (i : int64) (settings : FindSettings) -> settings.MaxSize <- i));
            ("minsize", (fun (i : int64) (settings : FindSettings) -> settings.MinSize <- i));
        ] |> Map.ofList

    let LoadOptionsFromJson (jsonString : string) : FindOption list =
        let findOptionsDict = JsonSerializer.Deserialize<FindOptionsDictionary>(jsonString)
        let optionDicts = findOptionsDict["findoptions"]
        [ for optionDict in optionDicts do
            let longArg = optionDict["long"]
            let shortArg = if optionDict.ContainsKey("short") then optionDict["short"] else ""
            let desc = optionDict["desc"]
            let argType =
                if boolActionMap.ContainsKey(longArg) then ArgTokenType.Bool
                elif stringActionMap.ContainsKey(longArg) then ArgTokenType.Str
                elif intActionMap.ContainsKey(longArg) then ArgTokenType.Int
                elif longActionMap.ContainsKey(longArg) then ArgTokenType.Long
                else ArgTokenType.Unknown
            yield { ShortArg=shortArg; LongArg=longArg; Description=desc; ArgType=argType } ]

    let _findOptionsResource = EmbeddedResource.GetResourceFileContents(config.FindOptionsPath);
    let options = LoadOptionsFromJson(_findOptionsResource)

    let argTokenizer : ArgTokenizer =
        ArgTokenizer(options)

    member this.Options = options
    
    member this.ApplyArgTokenToSettings (argToken : ArgToken) (settings : FindSettings) : Result<FindSettings, string> =
        if argToken.Type = ArgTokenType.Bool then
            if boolActionMap.ContainsKey(argToken.Name) then
                match argToken.Value with
                | :? Boolean as b ->
                    boolActionMap[argToken.Name] b settings
                    if argToken.Name = "defaultfiles" then
                        this.UpdateSettingsFromDefaultFiles settings
                    else
                        Ok settings
                | _ -> Error $"Invalid value for option: {argToken.Name}"
            else
                Error $"Invalid value for option: {argToken.Name}"
        elif argToken.Type = ArgTokenType.Str then
            if stringActionMap.ContainsKey(argToken.Name) then
                match argToken.Value with
                | :? string as s ->
                    stringActionMap[argToken.Name] s settings
                    Ok settings
                | _ -> Error $"Invalid value for option: {argToken.Name}"
            else
                Error $"Invalid value for option: {argToken.Name}"
        elif argToken.Type = ArgTokenType.Int then
            if intActionMap.ContainsKey(argToken.Name) then
                match argToken.Value with
                | :? int as i ->
                    intActionMap[argToken.Name] i settings
                    Ok settings
                | _ -> Error $"Invalid value for option: {argToken.Name}"
            else
                Error $"Invalid value for option: {argToken.Name}"
        elif argToken.Type = ArgTokenType.Long then
            if longActionMap.ContainsKey(argToken.Name) then
                match argToken.Value with
                | :? int64 as l ->
                    longActionMap[argToken.Name] l settings
                    Ok settings
                | _ -> Error $"Invalid value for option: {argToken.Name}"
            else
                Error $"Invalid value for option: {argToken.Name}"
        else
            Error $"Invalid option: {argToken.Name}"

    member this.UpdateSettingsFromArgTokens (settings : FindSettings) (argTokens : ArgToken list) : Result<FindSettings, string> =
        let rec recSettingsFromArgTokens (tokens : ArgToken list) (settings : FindSettings) : Result<FindSettings, string> =
            match tokens with
            | [] -> Ok settings
            | token :: tail ->
                match this.ApplyArgTokenToSettings token settings with
                | Ok settings ->
                    if token.Name = "help" then
                        Ok settings
                    else
                        recSettingsFromArgTokens tail settings
                | Error e -> Error e
        recSettingsFromArgTokens argTokens settings

    member this.UpdateSettingsFromDictionary (settings : FindSettings) (dict : Dictionary<string, obj>) : Result<FindSettings, string> =
        match argTokenizer.TokenizeDictionary(dict) with
        | Ok argTokens -> this.UpdateSettingsFromArgTokens settings argTokens
        | Error e -> Error e

    member this.UpdateSettingsFromJson (settings : FindSettings) (jsonString : string) : Result<FindSettings, string> =
        match argTokenizer.TokenizeJson(jsonString) with
        | Ok argTokens -> this.UpdateSettingsFromArgTokens settings argTokens
        | Error e -> Error e

    member this.SettingsFromJson (jsonString : string) : Result<FindSettings, string> =
        let settings = FindSettings()
        this.UpdateSettingsFromJson settings jsonString

    member this.UpdateSettingsFromFile (settings : FindSettings) (filePath : string) : Result<FindSettings, string> =
        match argTokenizer.TokenizeFile(filePath) with
        | Ok argTokens -> this.UpdateSettingsFromArgTokens settings argTokens
        | Error e -> Error e

    member this.SettingsFromFile (filePath : string) : Result<FindSettings, string> =
        let settings = FindSettings()
        this.UpdateSettingsFromFile settings filePath

    member this.UpdateSettingsFromDefaultFiles (settings : FindSettings) : Result<FindSettings, string> =
        if Path.Exists(config.DefaultFindSettingsPath) then
            this.UpdateSettingsFromFile settings config.DefaultFindSettingsPath
        else
            Ok settings

    member this.UpdateSettingsFromArgs (settings : FindSettings) (args : string[]) : Result<FindSettings, string> =
        match argTokenizer.TokenizeArgs(args) with
        | Ok argTokens -> this.UpdateSettingsFromArgTokens settings argTokens
        | Error e -> Error e

    member this.SettingsFromArgs (args : string[]) : Result<FindSettings, string> =
        let settings = FindSettings()
        settings.PrintFiles <- true
        // if a defaultfiles option isn't included, go ahead and apply default files now
        if not (Array.contains "--defaultfiles" args) && not (Array.contains "--nodefaultfiles" args) then
            match this.UpdateSettingsFromDefaultFiles(settings) with
            | Ok settings ->
                this.UpdateSettingsFromArgs settings args
            | Error e -> Error e
        else
            this.UpdateSettingsFromArgs settings args

    member this.SortOption (o1 : FindOption) (o2 : FindOption) : int =
        let os1 = if o1.ShortArg <> "" then o1.ShortArg + "@" + o1.LongArg else o1.LongArg
        let os2 = if o2.ShortArg <> "" then o2.ShortArg + "@" + o2.LongArg else o2.LongArg
        String.Compare(os1, os2, StringComparison.OrdinalIgnoreCase)

    member this.GetUsageString () : string =
        let sortedOptions = options
                            |> List.sortWith this.SortOption
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

    member this.Usage (exitCode : int) : unit =
        let usageString = this.GetUsageString()
        printfn $"%s{usageString}\n"
        Environment.Exit(exitCode)
