namespace FsFind

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

module FileUtil = 

    let currentPath = "."
    let parentPath = ".."
    let forwardSlash = '/'
    let backSlash = '\\'
    let dirSeps = [| forwardSlash; backSlash |]
    let dotDirs = Set.ofList [
        currentPath; parentPath;
        currentPath + string forwardSlash; parentPath + string forwardSlash;
        currentPath + string backSlash; parentPath + string backSlash
    ]

    let GetHomePath () : string = 
        match Environment.GetEnvironmentVariable("HOME") with
        | home when home <> null -> home
        | _ -> Environment.GetEnvironmentVariable("USERPROFILE")

    let GetFileContents (filePath : string) (encoding : Encoding) : string =
        let contents =
            try
                use sr = new StreamReader (filePath, encoding)
                sr.ReadToEnd()
            with
            | :? IOException as e -> printfn $"%s{e.Message}"; ""
        contents

    let GetFileLines (filePath : string) (encoding : Encoding) = seq {
        use sr = new StreamReader (filePath, encoding)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let NormalizePath (path : string) : string = 
        path.TrimEnd(dirSeps)

    let JoinPath (path1 : string) (path2 : string) : string = 
        let dirSep =
            if path1.IndexOf(backSlash) > -1 then backSlash else forwardSlash
        let p2 =
            if path2[0] = forwardSlash || path2[0] = backSlash then path2.Substring(1) else path2
        String.Format("{0}{1}{2}", NormalizePath path1, dirSep, p2)

    let ExpandPath (filePath : string) : string =
        if filePath[0] = '~' then JoinPath (GetHomePath()) (filePath.Substring(1))
        else filePath

    let ContractPath (filePath : string) : string =
        if filePath[0] = '~' then filePath 
        else filePath.Replace(GetHomePath(), "~")

    let GetRelativePath (fullPath : string) (startPath : string) : string =
        let startFullPath = NormalizePath (DirectoryInfo startPath).FullName
        let normStartPath = NormalizePath startPath
        if startFullPath <> normStartPath
        then fullPath.Replace (startFullPath, normStartPath)
        else fullPath

    let ContractOrRelativePath (fullPath : string) (startPath : string) : string =
        if startPath[0] = '~' then ContractPath fullPath 
        else GetRelativePath fullPath startPath
        
    let IsDotDir (filePath : string): bool = dotDirs.Contains(filePath)

    let IsHidden (filePath : string) : bool = 
        let startsWithDot = filePath[0] = '.' && not (IsDotDir filePath)
        //let hasHiddenAttribute = f.Exists && (f.Attributes &&& FileAttributes.Hidden) <> 0
        startsWithDot

    let IsHiddenFile (f : FileSystemInfo) : bool = 
        (f.Name[0] = '.' && not (IsDotDir f.Name)) ||
        (f.Exists && (f.Attributes &&& FileAttributes.Hidden) = FileAttributes.Hidden)

    let ExtensionsListFromString (exts : string) : string list =
        let nonWord = Regex(@"\W+")
        nonWord.Split(exts)
        |> Array.toList
        |> List.filter (fun (x : string) -> String.IsNullOrEmpty(x) = false)
        |> List.map (fun (x : string) -> if x.StartsWith(".") then x else "." + x)

;;
