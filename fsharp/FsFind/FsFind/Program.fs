namespace FsFind

module Main =

    let HandleError (err : string) : unit =
        Common.Log("");
        Common.LogError err
        FindOptions.Usage(1)

    let Find (settings : FindSettings) : unit =
        let finder = Finder(settings)

        let errs = finder.ValidateSettings()
        if errs.Length > 0 then
            HandleError errs.Head

        let files = finder.Find()

        if settings.ListDirs then
            finder.PrintMatchingDirs files

        if settings.ListFiles then
            finder.PrintMatchingFiles files


    [<EntryPoint>]
    let Main (args : string[]) = 
        match (Array.toList args) with
        | [] -> HandleError "Startpath not defined"
        | _ ->
            let settings, err = FindOptions.SettingsFromArgs(args)

            if err.Length > 0 then
                HandleError err

            if settings.Debug then
                Common.Log $"settings: %s{settings.ToString}"

            if settings.PrintUsage then
                FindOptions.Usage(0)
            else
                Find settings

        // main entry point return
        0;;
