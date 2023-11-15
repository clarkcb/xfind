namespace FsFind

open System

module Logger = 
    let Log (msg : string) : unit =
        printfn $"%s{msg}"

    let LogError (msg : string) : unit =
        Console.Error.WriteLine $"ERROR: %s{msg}"

;;
