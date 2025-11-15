namespace FsFindLib

open System

module Logger = 
    let Log (msg : string) : unit =
        printfn $"%s{msg}"

    let LogErrorColor (msg : string) (colorize : bool) : unit =
        let err =
            if colorize then $"%s{Color.BoldRed}ERROR: %s{msg}%s{Color.Reset}"
            else $"ERROR: %s{msg}"
        Console.Error.WriteLine err

    let LogError (msg : string) : unit =
        LogErrorColor msg true

;;
