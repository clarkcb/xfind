namespace FsFindLib

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Json
open System.Text.RegularExpressions

type ArgTokenType = 
    | Bool = 0
    | Str = 1
    | Int = 2

type ArgToken = {
    Name : string;
    Type : ArgTokenType;
    Value : obj
}

type ArgTokenizer (boolMap: Map<string, string>, strMap: Map<string, string>, intMap: Map<string, string>) =

    let longArgWithValRegex = Regex("^--([a-zA-Z0-9-]+)=(.*)$")
    let longArgWithoutValRegex = Regex("^--([a-zA-Z0-9-]+)$")
    let shortArgsRegex = Regex("^-([a-zA-Z0-9]{2,})$")
    let shortArgRegex = Regex("^-([a-zA-Z0-9])$")

    let (|RegexMatch|_|) (regex:Regex) (arg:string) =            
        let m = regex.Match(arg)
        if m.Success then Some(m) else None

    member this.TokenizeArgs (args : string[]) : Result<ArgToken list, string> =
        let rec recTokenizeArgs (argList : string list) (argTokens : ArgToken list) : Result<ArgToken list, string> =
            match argList with
            | [] -> Ok argTokens
            | head :: tail ->
                match head with
                | RegexMatch longArgWithValRegex m ->
                    let longArg = m.Groups[1].Value
                    let argVal = m.Groups[2].Value
                    recTokenizeArgs (List.append ["--" + longArg; argVal] tail) argTokens
                | RegexMatch longArgWithoutValRegex m ->
                    let longArg = m.Groups[1].Value
                    if boolMap.ContainsKey(longArg) then
                        if longArg = "help" then
                            recTokenizeArgs [] [{ Name="help"; Type=ArgTokenType.Bool; Value=true }]
                        else
                            recTokenizeArgs tail (List.append argTokens [{ Name=longArg; Type=ArgTokenType.Bool; Value=true }])
                    elif strMap.ContainsKey(longArg) || intMap.ContainsKey(longArg) || longArg.Equals("settings-file") then
                        match tail with
                        | [] ->
                            Error $"Missing value for option: %s{longArg}"
                        | aHead :: aTail ->
                            if strMap.ContainsKey(longArg) then
                                recTokenizeArgs aTail (List.append argTokens [{ Name=longArg; Type=ArgTokenType.Str; Value=aHead }])
                            elif intMap.ContainsKey(longArg) then
                                recTokenizeArgs aTail (List.append argTokens [{ Name=longArg; Type=ArgTokenType.Int; Value=(int aHead) }])
                            else
                                match this.TokenizeFile aHead with
                                | Ok settingsFileTokens -> recTokenizeArgs aTail (List.append argTokens settingsFileTokens)
                                | Error e -> Error e
                    else
                        Error $"Invalid option: %s{longArg}"
                | RegexMatch shortArgsRegex m ->
                    let shortArgs = m.Groups[1].Value
                    let shortArgsList = shortArgs |> Seq.map (fun c -> "-" + string c) |> List.ofSeq
                    recTokenizeArgs (List.append shortArgsList tail) argTokens
                | RegexMatch shortArgRegex m ->
                    let shortArg = m.Groups[1].Value
                    if boolMap.ContainsKey(shortArg) then
                        let longArg = boolMap[shortArg]
                        recTokenizeArgs (List.append ["--" + longArg] tail) argTokens
                    elif strMap.ContainsKey(shortArg) then
                        let longArg = strMap[shortArg]
                        recTokenizeArgs (List.append ["--" + longArg] tail) argTokens
                    elif intMap.ContainsKey(shortArg) then
                        let longArg = intMap[shortArg]
                        recTokenizeArgs (List.append ["--" + longArg] tail) argTokens
                    else
                        Error $"Invalid option: %s{shortArg}"
                | _ ->
                    recTokenizeArgs tail (List.append argTokens [{ Name="path"; Type=ArgTokenType.Str; Value=head }])
        recTokenizeArgs (Array.toList args) []

    member this.TokenizeJsonElem (name : string) (jsonElem : JsonElement) : Result<ArgToken list, string> =
        if jsonElem.ValueKind = JsonValueKind.False || jsonElem.ValueKind = JsonValueKind.True then
            let b = jsonElem.GetBoolean()
            Ok [{ Name=name; Type=ArgTokenType.Bool; Value=b }]
        elif jsonElem.ValueKind = JsonValueKind.String then
            let s = jsonElem.GetString()
            if s = null then
                Error $"Invalid value for option: {name}"
            elif s = "settings-file" then
                this.TokenizeFile s
            else
                Ok [{ Name=name; Type=ArgTokenType.Str; Value=s }]
        elif jsonElem.ValueKind = JsonValueKind.Array then
            let rec recTokenizeJsonElems (elems : JsonElement list) (argTokens : ArgToken list) : Result<ArgToken list, string> =
                match elems with
                | [] -> Ok argTokens
                | elem :: tail ->
                    if elem.ValueKind = JsonValueKind.String then
                        let s = elem.GetString()
                        if s = null then
                            Error $"Invalid value for option: {name}"
                        // TODO: add settings-file handling here
                        else
                            recTokenizeJsonElems tail (List.append argTokens [{ Name=name; Type=ArgTokenType.Str; Value=s }])
                    else
                        Error $"Invalid value for option: {name}"
            let elems = jsonElem.EnumerateArray() |> List.ofSeq
            recTokenizeJsonElems elems []
        elif jsonElem.ValueKind = JsonValueKind.Number then
            let i = jsonElem.GetInt32()
            Ok [{ Name=name; Type=ArgTokenType.Int; Value=i }]
        else Error $"Invalid value for option: {name}"

    member this.TokenizeDictionary (dictionary : Dictionary<string, obj>) : Result<ArgToken list, string> =
        let rec recTokenizeDictionary (keys : string list) (dict : Dictionary<string, obj>) (argTokens : ArgToken list) : Result<ArgToken list, string> =
            match keys with
            | [] -> Ok argTokens
            | k :: tail ->
                match dict[k] with
                | :? JsonElement as jsonElem ->
                    match this.TokenizeJsonElem k jsonElem with
                    | Ok jsonElemTokens -> recTokenizeDictionary tail dict (List.append argTokens jsonElemTokens)
                    | Error e -> Error e
                | :? Boolean as b ->
                    recTokenizeDictionary tail dict (List.append argTokens [{ Name=k; Type=ArgTokenType.Bool; Value=b }])
                | :? string as s ->
                    if k = "settings-file" then
                        match this.TokenizeFile s with
                        | Ok settingsFileTokens -> recTokenizeDictionary tail dict (List.append argTokens settingsFileTokens)
                        | Error e -> Error e
                    else
                        recTokenizeDictionary tail dict (List.append argTokens [{ Name=k; Type=ArgTokenType.Str; Value=s }])
                | :? (string list) as strings ->
                    if k = "settings-file" then
                        match this.TokenizeFiles strings with
                        | Ok settingsFilesTokens -> recTokenizeDictionary tail dict (List.append argTokens settingsFilesTokens)
                        | Error e -> Error e
                    else
                        let newTokens = [ for s in strings do yield { Name=k; Type=ArgTokenType.Str; Value=s } ]
                        recTokenizeDictionary tail dict (List.append argTokens newTokens)
                | :? int as i ->
                    recTokenizeDictionary tail dict (List.append argTokens [{ Name=k; Type=ArgTokenType.Int; Value=i }])
                | _ -> Error $"Invalid value for option: {k}"

        // keys are sorted so that output is consistent across all versions
        let keys = dictionary.Keys |> List.ofSeq |> List.sort
        recTokenizeDictionary keys dictionary []

    member this.TokenizeJson (jsonString : string) : Result<ArgToken list, string> =
        match JsonSerializer.Deserialize<Dictionary<string, obj>>(jsonString) with
        | null -> Error "Unable to parse json"
        | settingsDict -> this.TokenizeDictionary settingsDict

    member this.TokenizeFile (filePath : string) : Result<ArgToken list, string> =
        let expandedPath = FileUtil.ExpandPath(filePath)
        let fileInfo = FileInfo(expandedPath)
        if fileInfo.Exists then
            if fileInfo.Extension.Equals(".json") then
                let contents = FileUtil.GetFileContents expandedPath Encoding.Default
                this.TokenizeJson contents
            else
                Error $"Invalid settings file (must be JSON): {filePath}"
        else
            Error $"Settings file not found: {filePath}"

    member this.TokenizeFiles (filePaths : string list) : Result<ArgToken list, string> =
        let rec recTokenizeFiles (filePaths : string list) (argTokens : ArgToken list) : Result<ArgToken list, string> =
            match filePaths with
            | [] -> Ok argTokens
            | fp :: tail ->
                match this.TokenizeFile fp with
                | Ok fileTokens -> recTokenizeFiles tail (List.append argTokens fileTokens)
                | Error e -> Error e
        recTokenizeFiles filePaths []

;;
