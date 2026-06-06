namespace FsFindLib

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

module FileUtil = 

    let currentPath = "."
    let parentPath = ".."
    let dotDirs = Set.ofList [currentPath; parentPath]

    let GetHomePath () : string = 
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

    let GetFilePathExtension (filePath: string): string =
        let fileName = Path.GetFileName(filePath)
        if String.IsNullOrEmpty(fileName) || not (fileName.Contains(".")) || fileName.LastIndexOf(".") < 1 then ""
        else
            let ext = Path.GetExtension(fileName)
            if String.IsNullOrEmpty(ext) then ""
            // else ext.Substring(1)
            else ext

    let NormalizePath (path : string) : string = 
        path.TrimEnd(Path.DirectorySeparatorChar)

    let ExpandPath (filePath : string) : string =
        match filePath with
        | fp when fp = null -> ""
        | fp when fp = "" -> ""
        | fp when fp[0] = '~' ->
            let userPath = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            if fp = "~" || fp = "~/"
            then userPath
            else
                if fp.StartsWith("~/")
                then Path.Join(userPath, filePath.Substring(2))
                else
                    let homePath = Path.GetDirectoryName(userPath)
                    Path.Join(homePath, filePath.Substring(1))
        | _ -> filePath

    let IsDirectory (filePath : string) : bool =
        Directory.Exists(filePath) ||
        Directory.Exists(ExpandPath(filePath))

    let IsFile (filePath : string) : bool =
        File.Exists(filePath) ||
        File.Exists(ExpandPath(filePath))

    let IsSymlink (filePath : string) : bool =
        FileInfo(filePath).LinkTarget <> null

    let Exists (filePath : string) : bool =
        IsDirectory filePath ||
        IsFile filePath

    let GetRelativePath (fullPath : string) (startPath : string) : string =
        let startFullPath = NormalizePath (DirectoryInfo startPath).FullName
        let normStartPath = NormalizePath startPath
        if startFullPath <> normStartPath
        then fullPath.Replace (startFullPath, normStartPath)
        else fullPath

    let GetDirPathElems (dirPath: string) : string list =
        dirPath.Split(Path.DirectorySeparatorChar) |> Array.toList

    let IsDotDirPath (dirPath: string): bool = dotDirs.Contains(NormalizePath dirPath)

    let IsHiddenName (name : string) : bool = 
        //let hasHiddenAttribute = f.Exists && (f.Attributes &&& FileAttributes.Hidden) <> 0
        name.Length > 1 && name[0] = '.' && not (IsDotDirPath name)

    let rec IsHiddenPath (path : string) : bool = 
        let elems = GetDirPathElems path
        if List.isEmpty elems then false
        else List.contains true (List.map IsHiddenName elems)

    let GetFileContents (filePath : string) (encoding : Encoding) : string =
        let contents =
            try
                use sr = new StreamReader (filePath, encoding)
                sr.ReadToEnd()
            with
            | :? IOException as e -> printfn $"%s{e.Message}"; ""
        contents

    let ExtensionsListFromString (exts : string) : string list =
        let nonWord = Regex(@"\W+")
        nonWord.Split(exts)
        |> Array.toList
        |> List.filter (fun (x : string) -> String.IsNullOrEmpty(x) = false)
        |> List.map (fun (x : string) -> if x.StartsWith(".") then x else "." + x)

;;
