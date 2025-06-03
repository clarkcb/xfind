namespace FsFindLib

open System.IO
open System.Text.RegularExpressions

type FileResultFormatter (settings : FindSettings) =

    let Colorize (s : string) (matchStartIndex : int) (matchEndIndex : int) : string =
        let prefix =
            if matchStartIndex > 0
            then s.Substring(0, matchStartIndex)
            else ""
        let suffix =
            if matchEndIndex < s.Length
            then s.Substring(matchEndIndex)
            else ""
        let matchLength = matchEndIndex - matchStartIndex
        prefix +
            Color.Green + 
            s.Substring(matchStartIndex, matchLength) +
            Color.Reset + 
            suffix

    member this.FormatDirectoryWithColor (dir : DirectoryInfo) : string =
        if dir = null
        then "."
        else
            let dirName = dir.ToString()
            match (Seq.tryFind (fun p -> (p:Regex).Match(dirName).Success) settings.InDirPatterns) with
            | Some dirPattern ->
                let dirMatch = dirPattern.Match(dirName)
                Colorize dirName dirMatch.Index (dirMatch.Index + dirMatch.Length)
            | None -> dirName

    member this.FormatDirectory (dir : DirectoryInfo) : string =
        if settings.Colorize && not (List.isEmpty settings.InDirPatterns)
        then this.FormatDirectoryWithColor dir
        else dir.ToString()

    member this.FormatFileNameWithColor (fileName : string) : string =
        let formattedFileName =
            match (Seq.tryFind (fun p -> (p:Regex).Match(fileName).Success) settings.InFilePatterns) with
            | Some filePattern ->
                let fileMatch = filePattern.Match(fileName)
                Colorize fileName fileMatch.Index (fileMatch.Index + fileMatch.Length)
            | None -> fileName
        if List.isEmpty settings.InExtensions
        then formattedFileName
        else
            let idx = formattedFileName.LastIndexOf('.')
            if idx > 0 && idx < formattedFileName.Length - 1
            then Colorize formattedFileName (idx + 1) formattedFileName.Length
            else formattedFileName

    member this.FormatFileName (fileName : string) : string =
        if settings.Colorize && (not (List.isEmpty settings.InExtensions) || not (List.isEmpty settings.InFilePatterns))
        then this.FormatFileNameWithColor fileName
        else fileName

    member this.FormatFileResult (result : FileResult.t) : string =
        let parent = this.FormatDirectory result.File.Directory
        let fileName = this.FormatFileName result.File.Name
        Path.Join(parent, fileName)

;;
