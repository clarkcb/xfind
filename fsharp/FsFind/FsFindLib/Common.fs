namespace FsFind

open System

module Common = 
    let PrintElapsed (name : string) (ts : TimeSpan) : unit =
        let elapsedTime =
            String.Format("{0:00}:{1:00}:{2:00}.{3:00}",
                ts.Hours, ts.Minutes, ts.Seconds,
                ts.Milliseconds / 10)
        printfn $"Elapsed time for %s{name}: %s{elapsedTime}"

    let PrintNames (names : string list) : unit =
        for name in names do
            printfn $"Name: %s{name}"

    let ListToString (lst : 'a list) : string = 
        let rec RecListToString (acc : string) (lst : 'a list) =
            match lst with
            | []     -> acc.Trim()
            | [a]    -> (RecListToString (acc + " \"" + a.ToString() + "\"") [])
            | h :: t -> (RecListToString (acc + " \"" + h.ToString() + "\";") t) in
        sprintf "[%s]" (RecListToString "" lst)

;;
