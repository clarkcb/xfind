namespace FsFind

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

type Finder (settings : FindSettings.t) =
    let _fileTypes = FileTypes()
    let _results = List<FindResult.t>()
    let _fileSet = HashSet<FileInfo>()
    let _binaryEncoding = Encoding.GetEncoding("ISO-8859-1")
    let mutable _textFileEncoding = Encoding.GetEncoding("utf-8")

    // read-only member properties
    member this.Results = _results
    member this.FileSet = _fileSet
    member this.BinaryEncoding = _binaryEncoding

    // read-write member properties
    member this.TextFileEncoding 
        with get () = _textFileEncoding
        and set enc = _textFileEncoding <- enc

    // member methods
    member this.EncodingFromStringOrError (encName : string) : string option =
        try
            this.TextFileEncoding <- Encoding.GetEncoding(encName)
            None
        with
        | :? ArgumentException ->
            Some (sprintf "Invalid encoding: %s" encName)

    member this.ValidateSettings () : string list =
        [
            (if String.IsNullOrEmpty settings.StartPath then (Some "Startpath not defined") else None);
            (if Directory.Exists(settings.StartPath) || File.Exists(settings.StartPath) then None else (Some "Startpath not found"));
            (if List.isEmpty settings.FindPatterns then (Some "No find patterns defined") else None);
            (if settings.LinesBefore < 0 then (Some "Invalid linesbefore") else None);
            (if settings.LinesAfter < 0 then (Some "Invalid linesafter") else None);
            (if settings.MaxLineLength < 0 then (Some "Invalid maxlinelength") else None);
            (this.EncodingFromStringOrError settings.TextFileEncoding);
        ]
        |> List.filter (fun e -> e.IsSome)
        |> List.map (fun e -> e.Value)

    member this.MatchesAnyPattern (s : string) (patterns : Regex list) : bool =
        Seq.exists (fun p -> (p:Regex).Match(s).Success) patterns

    member this.AnyMatchesAnyPattern (slist : string seq) (patterns : Regex list) : bool =
        Seq.exists (fun s -> this.MatchesAnyPattern s patterns) slist

    member this.IsFindDir (d : DirectoryInfo) : bool =
        let elems = d.FullName.Split('/', '\\') |> Seq.filter (fun s -> not (String.IsNullOrEmpty s))
        (not settings.ExcludeHidden ||
         not (Seq.exists (fun e -> FileUtil.IsHidden e) elems)) &&
        (Seq.isEmpty settings.InDirPatterns ||
         this.AnyMatchesAnyPattern elems settings.InDirPatterns) &&
        (Seq.isEmpty settings.OutDirPatterns ||
         not (this.AnyMatchesAnyPattern elems settings.OutDirPatterns))

    member this.IsFindFile (f : FindFile.t) : bool =
        (List.isEmpty settings.InExtensions ||
         List.exists (fun x -> x = f.File.Extension) settings.InExtensions) &&
        (List.isEmpty settings.OutExtensions ||
         not (List.exists (fun x -> x = f.File.Extension) settings.OutExtensions)) &&
        (List.isEmpty settings.InFilePatterns ||
         List.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.InFilePatterns) &&
        (List.isEmpty settings.OutFilePatterns ||
         not (List.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.OutFilePatterns)) &&
        (List.isEmpty settings.InFileTypes ||
         List.exists (fun ft -> ft = f.FileType) settings.InFileTypes) &&
        (List.isEmpty settings.OutFileTypes ||
         not (List.exists (fun ft -> ft = f.FileType) settings.OutFileTypes))

    member this.IsArchiveFindFile (f : FindFile.t) : bool =
        (Seq.isEmpty settings.InArchiveExtensions ||
         Seq.exists (fun x -> x = f.File.Extension) settings.InArchiveExtensions) &&
        (Seq.isEmpty settings.OutArchiveExtensions ||
         not (Seq.exists (fun x -> x = f.File.Extension) settings.OutArchiveExtensions)) &&
        (Seq.isEmpty settings.InArchiveFilePatterns ||
         Seq.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.InArchiveFilePatterns) &&
        (Seq.isEmpty settings.OutArchiveFilePatterns ||
         not (Seq.exists (fun p -> (p:Regex).Match(f.File.Name).Success) settings.OutArchiveFilePatterns))

    member this.FilterFile (f: FindFile.t) : bool = 
        if FileUtil.IsHiddenFile f.File && settings.ExcludeHidden then
            false
        else if f.FileType = FileType.Archive then
            settings.FindArchives && this.IsArchiveFindFile f
        else
            not settings.ArchivesOnly && this.IsFindFile f

    member this.GetFindFiles () : FindFile.t list =
        let expandedPath = FileUtil.ExpandPath settings.StartPath
        let findOption =
            if settings.Recursive then FindOption.AllDirectories
            else FindOption.TopDirectoryOnly
        let dir = DirectoryInfo(expandedPath)
        dir.EnumerateFiles("*", findOption)
        |> Seq.filter (fun f -> this.IsFindDir(f.Directory))
        |> Seq.map (fun f -> FindFile.Create f (_fileTypes.GetFileType f))
        |> Seq.filter (fun sf -> this.FilterFile sf)
        |> List.ofSeq

    member this.AddFindResult (findResult : FindResult.t) : unit =
        _results.Add(findResult)

    member this.recGetPatternMatches (patterns : Regex list) (s : string) (pMatches : (Regex * Match) list) = 
        match patterns with
        | [] -> pMatches
        | p :: ps ->
            let allMatches = [for m in p.Matches(s) do yield m]
            let nextMatches =
                if allMatches.IsEmpty
                then []
                else
                    if settings.FirstMatch
                    then allMatches |> List.take 1
                    else allMatches
            let nextPMatches = List.map (fun m -> (p, m)) nextMatches
            this.recGetPatternMatches ps s (List.append pMatches nextPMatches)

    member this.GetLineIndices (s : string) : (int * int) list =
        let newLineIndices =
            Array.zip [|0 .. s.Length - 1|] (s.ToCharArray())
            |> Array.filter (fun x -> snd x = '\n')
            |> Array.map (fun x -> fst x)
            |> List.ofArray
        let startLineIndices =
             0 :: List.map (fun i -> i + 1) newLineIndices
             |> Seq.takeWhile (fun i -> i < s.Length)
             |> List.ofSeq
        //Common.Log (sprintf "startLineIndices (%d): [%s]" startLineIndices.Length (String.Join(",", startLineIndices)))
        let endLineIndices = newLineIndices
        //Common.Log (sprintf "endLineIndices (%d): [%s]" endLineIndices.Length (String.Join(",", endLineIndices)))
        if endLineIndices.Length < startLineIndices.Length then
            List.zip startLineIndices (endLineIndices @ [s.Length - 1])
        else
            List.zip startLineIndices endLineIndices

    member this.LinesMatch (lines : string seq, inPatterns : Regex list,
                            outPatterns : Regex list) : bool =
        (List.isEmpty inPatterns || (this.AnyMatchesAnyPattern lines inPatterns))
        &&
        (List.isEmpty outPatterns || (not (this.AnyMatchesAnyPattern lines outPatterns)))

    member this.LinesBeforeMatch (linesBefore : string seq) : bool =
        this.LinesMatch (linesBefore, settings.InLinesBeforePatterns,
                         settings.OutLinesBeforePatterns)

    member this.LinesAfterMatch (linesAfter : string seq) : bool =
        this.LinesMatch(linesAfter, settings.InLinesAfterPatterns,
                        settings.OutLinesAfterPatterns)

    member this.TakeRight (ss : 'T list) (num : int) : 'T list =
        if ss.Length < num then
            ss
        else
            ss
            |> List.ofSeq
            |> List.rev
            |> Seq.take num
            |> List.ofSeq
            |> List.rev

    member this.FindContents (s : string) : List<FindResult.t> =
//        let patternMatches = Dictionary<Regex, int>()
        let results = List<FindResult.t>()
        let lineIndices = this.GetLineIndices s
        for p in settings.FindPatterns do
            let mutable m = p.Match s
            let mutable stop = false
            while m.Success && not stop do
                let matchStartIndex = m.Index
                let matchEndIndex = m.Index + m.Length
                let startLineIndex = Seq.filter (fun i -> fst i < matchStartIndex) lineIndices |> Seq.max |> fst
                //Common.Log (sprintf "startLineIndex: %d" startLineIndex)
                let endLineIndex = Seq.filter (fun i -> snd i > matchStartIndex) lineIndices |> Seq.min |> snd
                //Common.Log (sprintf "endLineIndex: %d" endLineIndex)
                let beforeIndices = Seq.takeWhile (fun i -> fst i < startLineIndex) lineIndices |> List.ofSeq
                //Common.Log (Common.ListToString("beforeIndices", beforeIndices))
                let afterIndices = Seq.skipWhile (fun i -> fst i <= matchStartIndex) lineIndices |> List.ofSeq
                let lineNum = beforeIndices.Length + 1
                let line = s.Substring(startLineIndex, endLineIndex - startLineIndex)
                let beforeLineIndices = this.TakeRight beforeIndices settings.LinesBefore
                //Common.Log (Common.ListToString("beforeLineIndices", beforeLineIndices))
                let linesBefore =
                    if settings.LinesBefore > 0 then
                        beforeLineIndices
                        |> List.map (fun (x,y) -> s.Substring(x, y - x))
                    else
                        []
                let linesAfter =
                    if settings.LinesAfter > 0 then
                        afterIndices
                        |> Seq.take(settings.LinesAfter)
                        |> List.ofSeq
                        |> List.map (fun (x,y) -> s.Substring(x, y - x))
                    else
                        []
                if (linesBefore.Length = 0 || this.LinesBeforeMatch linesBefore) &&
                   (linesAfter.Length = 0 || this.LinesAfterMatch linesAfter) then
                    let r = FindResult.Create p lineNum (matchStartIndex - startLineIndex + 1) (matchEndIndex - startLineIndex + 1) line linesBefore linesAfter
                    results.Add(r)
                    if settings.FirstMatch then
                        stop <- true
                m <- m.NextMatch()
        results

    member this.FindTextFileContents (f : FindFile.t) : unit =
        try
            let contents = FileUtil.GetFileContents (f.File.FullName) (this.TextFileEncoding)
            let results = this.FindContents(contents)
            for r:FindResult.t in results do
                let fileResult = { r with File=f }
                this.AddFindResult fileResult
        with
        | :? IOException as e -> printfn "%s" e.Message

    member this.FindLine (line : string) (lineNum : int) (linesBefore : string list) (linesAfter : string list) : FindResult.t list =
        let pMatches = this.recGetPatternMatches settings.FindPatterns line []
        let toFindResult (p : Regex) (m : Match) = 
            FindResult.Create p lineNum (m.Index + 1) (m.Index + m.Length + 1) line linesBefore linesAfter
        let results : FindResult.t list = List.map (fun (p, m) -> toFindResult p m) pMatches
        results

    member this.FindLines (lines : string list) =
        let rec recFindLines (lineNum : int) (linesBefore : string list) (lines : string list) (results : FindResult.t list) =
            let nextLines =
                if settings.FirstMatch && (List.length results) = (List.length settings.FindPatterns)
                then []
                else lines
            match nextLines with
            | [] -> results
            | l :: ls ->
                let linesAfter =
                    if settings.LinesAfter > 0
                    then
                        if (List.length ls < settings.LinesAfter)
                        then ls
                        else (List.take settings.LinesAfter ls)
                    else []
                let rs = this.FindLine l lineNum linesBefore linesAfter
                let lbs = 
                    if settings.LinesBefore > 0
                    then
                        if (List.length linesBefore) = settings.LinesBefore
                        then (List.append (List.tail linesBefore) [l])
                        else (List.append linesBefore [l])
                    else []
                recFindLines (lineNum + 1) lbs ls (List.append results rs)
        recFindLines 1 [] lines []

    member this.FindTextFileLines (f : FindFile.t) : unit =
        try
            let lines = FileUtil.GetFileLines f.File.FullName this.TextFileEncoding |> List.ofSeq
            let results =
                this.FindLines lines
                |> List.map (fun r -> { r with File = f })
            for r:FindResult.t in results do
                this.AddFindResult r
        with
        | :? IOException as e -> printfn "%s" e.Message

    member this.FindTextFile (f : FindFile.t) : unit =
        if settings.Debug then
            Common.Log (sprintf "Finding text file %s" f.File.FullName)
        if settings.MultiLineFind then
            this.FindTextFileContents f
        else
            this.FindTextFileLines f

    member this.FindBlob (blob : string): FindResult.t list =
        let pMatches = this.recGetPatternMatches settings.FindPatterns blob []
        // if settings.debug then log_msg (sprintf "p_matches: %d" (List.length p_matches));
        let toFindResult (p : Regex) (m : Match) = 
            FindResult.Create p 0 (m.Index + 1) (m.Index + m.Length + 1) "" [] []
        let results : FindResult.t list = List.map (fun (p, m) -> toFindResult p m) pMatches
        results

    member this.FindBinaryFile (f : FindFile.t) : unit =
        if settings.Verbose then
            Common.Log (sprintf "Finding binary file %s" f.File.FullName)
        try
            use sr = new StreamReader (f.File.FullName, this.BinaryEncoding)
            let contents = sr.ReadToEnd()
            for p in Seq.filter (fun p -> (p:Regex).Match(contents).Success) settings.FindPatterns do
                let mutable m = p.Match contents
                let mutable stop = false
                while m.Success && not stop do
                    let matchStartIndex = m.Index + 1
                    let matchEndIndex = m.Index + m.Length + 1
                    let r = FindResult.Create p 0 matchStartIndex matchEndIndex null [] []
                    this.AddFindResult { r with File=f }
                    if settings.FirstMatch then
                        stop <- true
                    m <- m.NextMatch()
        with
        | :? IOException as ex -> printfn "%s" (ex.Message)

    member this.FindFile (f : FindFile.t) : unit =
        match f.FileType with
        | FileType.Archive -> Common.Log (sprintf "Archive file finding not currently supported")
        | FileType.Binary -> this.FindBinaryFile f
        | FileType.Code | FileType.Text | FileType.Xml -> this.FindTextFile f
        | FileType.Unknown -> Common.Log (sprintf "Skipping file of unknown type")
        | _ -> Common.Log (sprintf "Skipping file of indeterminate type (this shouldn't happen): %s" f.File.FullName)

    member this.FindPath : unit =
        let findFiles : FindFile.t list =
            this.GetFindFiles()
            |> List.filter (fun sf -> sf.File <> null)
            |> List.sortBy (fun sf -> sf.File.ToString())

        if settings.Verbose then
            let findDirs =
                findFiles
                |> List.filter (fun sf -> sf.File.Directory <> null)
                |> List.map (fun sf -> sf.File.Directory.ToString())
                |> List.distinct
                |> List.sort
            
            Common.Log (sprintf "\nDirectories to be found (%d):" findDirs.Length)
            List.iter (fun d -> Common.Log (sprintf "%s" (FileUtil.ContractOrRelativePath d settings.StartPath))) findDirs
        
            Common.Log (sprintf "\nFiles to be found (%d):" findFiles.Length)
            Seq.iter (fun (f: FindFile.t) -> Common.Log (sprintf "%s" (FileUtil.ContractOrRelativePath (f.File.ToString()) settings.StartPath))) findFiles

        for f in findFiles do
            this.FindFile f

    member this.Find () : unit =
        let expandedPath = FileUtil.ExpandPath(settings.StartPath)
        if Directory.Exists(expandedPath) then
            let startDir = DirectoryInfo(expandedPath)
            if this.IsFindDir startDir then
                this.FindPath
            else
                raise <| Exception ("Startpath does not match find settings")
        else
            let startFile = FileInfo(expandedPath)
            this.FindFile (FindFile.Create startFile (_fileTypes.GetFileType startFile))

    member this.GetSortedResults : FindResult.t list = 
        this.Results
        |> Seq.sortBy (fun r -> (r.File.File.ToString(), r.LineNum, r.MatchStartIndex, r.MatchEndIndex))
        |> List.ofSeq

    member this.PrintResults : unit =
        let formatter = FindResultFormatter(settings)
        Common.Log (sprintf "\nFind results (%d):" this.Results.Count)
        this.GetSortedResults
        |> Seq.iter (fun r -> Common.Log (sprintf "%s" (formatter.Format r)))

    member this.GetMatchingDirs : DirectoryInfo list = 
        this.Results
        |> Seq.map (fun r -> r.File.File.Directory)
        |> Seq.distinctBy (fun d -> d.FullName)
        |> Seq.sortBy (fun d -> d.FullName)
        |> List.ofSeq

    member this.PrintMatchingDirs : unit = 
        let dirs = this.GetMatchingDirs
        Common.Log (sprintf "\nDirectories with matches (%d):" dirs.Length)
        for d in dirs do
            printfn "%s" d.FullName

    member this.GetMatchingFiles : FileInfo list = 
        this.Results
        |> Seq.map (fun r -> r.File.File)
        |> Seq.distinctBy (fun f -> f.FullName)
        |> Seq.sortBy (fun f -> f.FullName)
        |> List.ofSeq

    member this.PrintMatchingFiles : unit = 
        let files = this.GetMatchingFiles
        Common.Log (sprintf "\nFiles with matches (%d):" files.Length)
        for f in files do
            printfn "%s" f.FullName

    member this.GetMatchingLines : string list = 
        let lines =
            this.Results
            |> Seq.map (fun r -> r.Line.Trim())
        if settings.UniqueLines then
            Seq.sortBy (fun (s : string) -> s.ToUpper()) lines
            |> Seq.distinct
            |> List.ofSeq
        else
            Seq.sortBy (fun (s : string) -> s.ToUpper()) lines
            |> List.ofSeq

    member this.PrintMatchingLines : unit = 
        let lines = this.GetMatchingLines
        let title =
            if settings.UniqueLines then "Unique lines with matches"
            else "Lines with matches"
        Common.Log (sprintf "\n%s (%d):" title lines.Length)
        for l in lines do
            printfn "%s" l

;;
