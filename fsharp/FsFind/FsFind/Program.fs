namespace FsFind

open FsFindLib

module Main =

    let HandleError (err : string) (colorize : bool) (findOptions : FindOptions) : unit =
        Logger.Log("");
        Logger.LogErrorColor err colorize
        findOptions.Usage(1)

    let Find (config : FindConfig) (findOptions : FindOptions) (settings : FindSettings) : unit =
        let finder = Finder(config, settings)

        let errs = finder.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head settings.Colorize findOptions

        match finder.Find() with
        | Ok files ->
            let formatter = FileResultFormatter(settings)

            if settings.PrintDirs then
                finder.PrintMatchingDirs files formatter

            if settings.PrintFiles then
                finder.PrintMatchingFiles files formatter
                
        | Error e -> HandleError e settings.Colorize findOptions


    [<EntryPoint>]
    let Main (args : string[]) =
        let config = FindConfig()
        let findOptions = FindOptions(config)
        match findOptions.SettingsFromArgs(args) with
        | Ok settings ->
            if settings.Debug then
                Logger.Log settings.ToString
            if settings.PrintUsage then
                findOptions.Usage(0)
            else
                Find config findOptions settings
        | Error e -> HandleError e true findOptions

        // main entry point return
        0;;
