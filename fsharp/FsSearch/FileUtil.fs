﻿namespace FsSearch

open System
open System.IO

module FileUtil = 

    let dotDirs = Set.ofList ["."; ".."]

    let GetHomePath () : string = 
        match Environment.GetEnvironmentVariable("HOME") with
        | home when home <> null -> home
        | _ -> Environment.GetEnvironmentVariable("USERPROFILE")

    let GetFileContents (filepath : string) =
        let contents =
            try
                use sr = new StreamReader (filepath)
                sr.ReadToEnd()
            with
            | :? IOException as e -> printfn "%s" e.Message; ""
        contents

    let ExpandPath (filepath : string) =
        if filepath.[0] = '~' then GetHomePath() + filepath.Substring(1)
        else filepath

    let IsDotDir (filepath : string) = dotDirs.Contains(filepath)

    let IsDirectory (filepath : string) =
        if IsDotDir filepath then true
        else
            try
                let attr = File.GetAttributes(filepath)
                attr &&& FileAttributes.Directory = FileAttributes.Directory
            with
            | :? DirectoryNotFoundException -> false
            | :? FileNotFoundException -> false

    let IsHidden (filepath : string) : bool = 
        let startsWithDot = filepath.[0] = '.' && not (IsDotDir filepath)
        //let hasHiddenAttribute = f.Exists && (f.Attributes &&& FileAttributes.Hidden) <> 0
        startsWithDot

    let GetFileLines (filePath : string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    ;;
