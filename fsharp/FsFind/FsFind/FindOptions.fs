﻿namespace FsFind

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
            ("encoding", (fun (s : string) (settings : FindSettings.t) -> { settings with TextFileEncoding = s }));
            ("in-archiveext", (fun (s : string) (settings : FindSettings.t) -> { settings with InArchiveExtensions = AddExtensions s settings.InArchiveExtensions }));
            ("in-archivefilepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with InArchiveFilePatterns = AddPattern s settings.InArchiveFilePatterns }));
            ("in-dirpattern", (fun (s : string) (settings : FindSettings.t) -> { settings with InDirPatterns = AddPattern s settings.InDirPatterns }));
            ("in-ext", (fun (s : string) (settings : FindSettings.t) -> { settings with InExtensions = AddExtensions s settings.InExtensions }));
            ("in-filepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with InFilePatterns = AddPattern s settings.InFilePatterns }));
            ("in-filetype", (fun (s : string) (settings : FindSettings.t) -> { settings with InFileTypes = AddFileTypes s settings.InFileTypes }));
            ("in-linesafterpattern", (fun (s : string) (settings : FindSettings.t) -> { settings with InLinesAfterPatterns = AddPattern s settings.InLinesAfterPatterns }));
            ("in-linesbeforepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with InLinesBeforePatterns = AddPattern s settings.InLinesBeforePatterns }));
            ("linesafter", (fun (s : string) (settings : FindSettings.t) -> { settings with LinesAfter = Int32.Parse(s) }));
            ("linesaftertopattern", (fun (s : string) (settings : FindSettings.t) -> { settings with LinesAfterToPatterns = AddPattern s settings.LinesAfterToPatterns }));
            ("linesafteruntilpattern", (fun (s : string) (settings : FindSettings.t) -> { settings with LinesAfterUntilPatterns = AddPattern s settings.LinesAfterUntilPatterns }));
            ("linesbefore", (fun (s : string) (settings : FindSettings.t) -> { settings with LinesBefore = Int32.Parse(s) }));
            ("maxlinelength", (fun (s : string) (settings : FindSettings.t) -> { settings with MaxLineLength = Int32.Parse(s) }));
            ("out-archiveext", (fun (s : string) (settings : FindSettings.t) -> { settings with OutArchiveExtensions = AddExtensions s settings.OutArchiveExtensions }));
            ("out-archivefilepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with OutArchiveFilePatterns = AddPattern s settings.OutArchiveFilePatterns }));
            ("out-dirpattern", (fun (s : string) (settings : FindSettings.t) -> { settings with OutDirPatterns = AddPattern s settings.OutDirPatterns }));
            ("out-ext", (fun (s : string) (settings : FindSettings.t) -> { settings with OutExtensions = AddExtensions s settings.OutExtensions }));
            ("out-filepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with OutFilePatterns = AddPattern s settings.OutFilePatterns }));
            ("out-filetype", (fun (s : string) (settings : FindSettings.t) -> { settings with OutFileTypes = AddFileTypes s settings.OutFileTypes }));
            ("out-linesafterpattern", (fun (s : string) (settings : FindSettings.t) -> { settings with OutLinesAfterPatterns = AddPattern s settings.OutLinesAfterPatterns }));
            ("out-linesbeforepattern", (fun (s : string) (settings : FindSettings.t) -> { settings with OutLinesBeforePatterns = AddPattern s settings.OutLinesBeforePatterns }));
            ("findpattern", (fun (s : string) (settings : FindSettings.t) -> { settings with FindPatterns = AddPattern s settings.FindPatterns }));
        ] |> Map.ofList

    let flagActionMap : Map<string, bool -> FindSettings.t -> FindSettings.t> =
        [
            ("allmatches", (fun (b : bool) (settings : FindSettings.t) -> { settings with FirstMatch = not b }));
            ("archivesonly", (fun (b : bool) (settings : FindSettings.t) -> FindSettings.SetArchivesOnly b settings));
            ("colorize", (fun (b : bool) (settings : FindSettings.t) -> { settings with Colorize = b }));
            ("debug", (fun (b : bool) (settings : FindSettings.t) -> FindSettings.SetDebug b settings));
            ("excludehidden", (fun (b : bool) (settings : FindSettings.t) -> { settings with ExcludeHidden = b }));
            ("firstmatch", (fun (b : bool) (settings : FindSettings.t) -> { settings with FirstMatch = b }));
            ("help", (fun (b : bool) (settings : FindSettings.t) -> { settings with PrintUsage = b }));
            ("includehidden", (fun (b : bool) (settings : FindSettings.t) -> { settings with ExcludeHidden = not b }));
            ("listdirs", (fun (b : bool) (settings : FindSettings.t) -> { settings with ListDirs = b }));
            ("listfiles", (fun (b : bool) (settings : FindSettings.t) -> { settings with ListFiles = b }));
            ("listlines", (fun (b : bool) (settings : FindSettings.t) -> { settings with ListLines = b }));
            ("multilineoption-REMOVE", (fun (b : bool) (settings : FindSettings.t) -> { settings with MultiLineFind = b }));
            ("nocolorize", (fun (b : bool) (settings : FindSettings.t) -> { settings with Colorize = not b }));
            ("noprintmatches", (fun (b : bool) (settings : FindSettings.t) -> { settings with PrintResults = not b }));
            ("norecursive", (fun (b : bool) (settings : FindSettings.t) -> { settings with Recursive = not b }));
            ("nofindarchives", (fun (b : bool) (settings : FindSettings.t) -> { settings with FindArchives = not b }));
            ("printmatches", (fun (b : bool) (settings : FindSettings.t) -> { settings with PrintResults = b }));
            ("recursive", (fun (b : bool) (settings : FindSettings.t) -> { settings with Recursive = b }));
            ("findarchives", (fun (b : bool) (settings : FindSettings.t) -> { settings with FindArchives = b }));
            ("uniquelines", (fun (b : bool) (settings : FindSettings.t) -> { settings with UniqueLines = b }));
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

    let OptionsFromXml (xmlString : string) : FindOption list =
        let rec recOptionsFromXml (nodeList : XElement list) (options : FindOption list) : FindOption list =
            match nodeList with
            | [] -> options
            | n :: ns ->
                let short = [for a in n.Attributes(XName.Get("short")) do yield a.Value].Head
                let long = [for a in n.Attributes(XName.Get("long")) do yield a.Value].Head
                let desc = n.Value.Trim()
                recOptionsFromXml ns (List.append options [{ ShortArg=short; LongArg=long; Description=desc }])
        let optNodes = XDocument.Parse(xmlString).Descendants(XName.Get("findoption"))
        recOptionsFromXml (List.ofSeq optNodes) []
        |> List.sortBy (fun o -> if (o.ShortArg <> "") then (o.ShortArg.ToLower() + "@" + o.LongArg) else o.LongArg)

//    let _findOptionsResource = EmbeddedResource.GetResourceFileContents("FsFind.Resources.findoptions.xml");
    let _findOptionsResource = EmbeddedResource.GetResourceFileContents("FsFind.Resources.findoptions.json");
//    let options = OptionsFromXml(_findOptionsResource)
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
                                settings, sprintf "Missing value for option: %s" opt
                            | aHead :: aTail -> 
                                recSettingsFromArgs aTail (argActionMap.[long] aHead settings)
                        elif flagActionMap.ContainsKey(long) then
                            if long = "help" then
                                recSettingsFromArgs [] (flagActionMap.[long] true settings)
                            else
                                recSettingsFromArgs tail (flagActionMap.[long] true settings)
                        else
                            settings, sprintf "Invalid option: %s" opt
                    else
                        settings, sprintf "Invalid option: %s" opt
                | _ -> recSettingsFromArgs tail { settings with StartPath = head }
        recSettingsFromArgs (Array.toList args) { FindSettings.DefaultSettings with PrintResults = true }

    let GetUsageString () : string =
        let optStringMap =
            [ for opt in options do
                let shortstring : string = 
                    if opt.ShortArg <> "" then "-" + opt.ShortArg + ","
                    else ""
                let longstring : string = "--" + opt.LongArg
                yield (opt.LongArg, shortstring + longstring) ]
            |> Map.ofList

        let optDescMap =
            [ for opt in options do
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
            [for o in options do
                yield String.Format(format, optStringMap.[o.LongArg], optDescMap.[o.LongArg])]

        let usageString = 
            usageStrings
            |> List.append ["\nUsage:"; " fsfind [options] -s <findpattern> <startpath>\n"; "Options:"] 
            |> String.concat "\n"
        usageString

    let Usage (exitCode : int) : unit =
        let usageString = GetUsageString()
        printfn "%s\n" usageString
        Environment.Exit(exitCode)
