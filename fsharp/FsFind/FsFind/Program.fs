namespace FsFind

open FsFindLib

module Main =

    let HandleError (err : string) : unit =
        Logger.Log("");
        Logger.LogError err
        FindOptions.Usage(1)

    let Find (settings : FindSettings) : unit =
        let finder = Finder(settings)

        let errs = finder.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head

        let files = finder.Find()
        let formatter = FileResultFormatter(settings)

        if settings.PrintDirs then
            finder.PrintMatchingDirs files formatter

        if settings.PrintFiles then
            finder.PrintMatchingFiles files formatter


    [<EntryPoint>]
    let Main (args : string[]) = 
        match (Array.toList args) with
        | [] -> HandleError "Startpath not defined"
        | _ ->
            match FindOptions.SettingsFromArgs(args) with
            | Ok settings ->
                if settings.Debug then
                    Logger.Log settings.ToString
                if settings.PrintUsage then
                    FindOptions.Usage(0)
                else
                    Find settings
            | Error e -> HandleError e

        // main entry point return
        0;;
