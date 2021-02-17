namespace FsFind

module Main =

    let HandleError (err : string) : unit =
        Common.Log (sprintf "\nERROR: %s" err)
        FindOptions.Usage(1)

    let Find (settings : FindSettings.t) : unit =
        let finder = Finder(settings)

        let errs = finder.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head

        finder.Find()

        if settings.PrintResults then
            finder.PrintResults

        if settings.ListDirs then
            finder.PrintMatchingDirs

        if settings.ListFiles then
            finder.PrintMatchingFiles

        if settings.ListLines then
            finder.PrintMatchingLines


    [<EntryPoint>]
    let Main(args : string[]) = 
        match (Array.toList args) with
        | [] -> HandleError "Startpath not defined"
        | _ ->
            let settings, err = FindOptions.SettingsFromArgs(args)

            if err.Length > 0 then
                HandleError err

            if settings.Debug then
                Common.Log (sprintf "settings: %s" (FindSettings.ToString settings))

            if settings.PrintUsage then
                FindOptions.Usage(0)
            else
                Find settings

        // main entry point return
        0;;
