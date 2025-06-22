namespace FsFindLib

open System.IO
open System.Text.RegularExpressions

type FileResultFormatter (settings : FindSettings) =

    member this.FormatDirectoryWithColor (dir : DirectoryInfo) : string =
        if dir = null
        then "."
        else
            let dirName = dir.ToString()
            match (Seq.tryFind (fun p -> (p:Regex).Match(dirName).Success) settings.InDirPatterns) with
            | Some dirPattern ->
                let dirMatch = dirPattern.Match(dirName)
                Color.Colorize dirName dirMatch.Index (dirMatch.Index + dirMatch.Length)
            | None -> dirName

    member this.FormatDirectoryFun =
        if settings.Colorize
        then this.FormatDirectoryWithColor
        else fun (dir : DirectoryInfo) -> dir.ToString()

    member this.FormatDirectory (dir : DirectoryInfo) : string =
        this.FormatDirectoryFun dir

    member this.FormatFileNameWithColor (fileName : string) : string =
        let formattedFileName =
            match (Seq.tryFind (fun p -> (p:Regex).Match(fileName).Success) settings.InFilePatterns) with
            | Some filePattern ->
                let fileMatch = filePattern.Match(fileName)
                Color.Colorize fileName fileMatch.Index (fileMatch.Index + fileMatch.Length)
            | None -> fileName
        if List.isEmpty settings.InExtensions
        then formattedFileName
        else
            let idx = formattedFileName.LastIndexOf('.')
            if idx > 0 && idx < formattedFileName.Length - 1
            then Color.Colorize formattedFileName (idx + 1) formattedFileName.Length
            else formattedFileName

    member this.FormatFileNameFun =
        if settings.Colorize
        then this.FormatFileNameWithColor
        else fun (fileName : string) -> fileName

    member this.FormatFileName (fileName : string) : string =
        this.FormatFileNameFun fileName

    member this.FormatFileResult (result : FileResult.t) : string =
        let parent = this.FormatDirectory result.File.Directory
        let fileName = this.FormatFileName result.File.Name
        Path.Join(parent, fileName)

;;
