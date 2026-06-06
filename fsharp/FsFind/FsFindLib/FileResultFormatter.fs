namespace FsFindLib

open System.IO
open System.Text.RegularExpressions

type FileResultFormatter (settings : FindSettings) =

    member this.FormatDirPathWithColor (dirPath: string) : string =
        if dirPath = null
        then "."
        else
            // let dirName = dirPath.ToString()
            match (Seq.tryFind (fun p -> (p:Regex).Match(dirPath).Success) settings.InDirPatterns) with
            | Some dirPattern ->
                let dirMatch = dirPattern.Match(dirPath)
                ColorUtil.Colorize dirPath dirMatch.Index (dirMatch.Index + dirMatch.Length) settings.DirColor
            | None -> dirPath

    member this.FormatDirPathFun =
        if settings.Colorize
        then this.FormatDirPathWithColor
        else fun (dirPath : string) -> dirPath

    member this.FormatDirPath (dirPath : string) : string =
        this.FormatDirPathFun dirPath

    member this.FormatFileNameWithColor (fileName : string) : string =
        let formattedFileName =
            match (Seq.tryFind (fun p -> (p:Regex).Match(fileName).Success) settings.InFilePatterns) with
            | Some filePattern ->
                let fileMatch = filePattern.Match(fileName)
                ColorUtil.Colorize fileName fileMatch.Index (fileMatch.Index + fileMatch.Length) settings.FileColor
            | None -> fileName
        if List.isEmpty settings.InExtensions
        then formattedFileName
        else
            let idx = formattedFileName.LastIndexOf('.')
            if idx > 0 && idx < formattedFileName.Length - 1
            then ColorUtil.Colorize formattedFileName (idx + 1) formattedFileName.Length settings.ExtColor
            else formattedFileName

    member this.FormatFileNameFun =
        if settings.Colorize
        then this.FormatFileNameWithColor
        else fun (fileName : string) -> fileName

    member this.FormatFileName (fileName : string) : string =
        this.FormatFileNameFun fileName

    member this.FormatFileResult (result : FileResult.t) : string =
        let parent = this.FormatDirPath (Path.GetDirectoryName result.FilePath)
        let fileName = this.FormatFileName (Path.GetFileName result.FilePath)
        Path.Join(parent, fileName)

;;
