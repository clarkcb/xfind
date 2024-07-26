namespace FsFind

open System


module FindConfig =
    let home : string = 
        match Environment.GetEnvironmentVariable("HOME") with
        | home when home <> null -> home
        | _ -> Environment.GetEnvironmentVariable("USERPROFILE")
    let XfindPath =
        match Environment.GetEnvironmentVariable("XFIND_PATH") with
        | xfindPath when xfindPath <> null -> xfindPath
        | _ -> home + "/src/xfind"
    let XfindDb = XfindPath + "/shared/xfind.db"

;;
