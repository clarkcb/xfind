namespace FsFind

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

    let GetFileExtension (fi : FileInfo): string =
        let ext = fi.Extension
        if String.IsNullOrEmpty(ext) then ""
        else ext.Substring(1)        

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

    let Exists (filePath : string) : bool =
        Directory.Exists(filePath) ||
        Directory.Exists(ExpandPath(filePath)) ||
        File.Exists(filePath) ||
        File.Exists(ExpandPath(filePath))
    
    let GetRelativePath (fullPath : string) (startPath : string) : string =
        let startFullPath = NormalizePath (DirectoryInfo startPath).FullName
        let normStartPath = NormalizePath startPath
        if startFullPath <> normStartPath
        then fullPath.Replace (startFullPath, normStartPath)
        else fullPath

    let GetDirElems (dir : DirectoryInfo) : string list =
        let mutable elems = [dir.Name]
        let mutable parent = dir.Parent
        let root = Path.DirectorySeparatorChar.ToString()
        while parent <> null && parent.Name <> root do
            elems <- parent.Name :: elems
            parent <- parent.Parent
        elems

    let IsDotDir (filePath : string): bool = dotDirs.Contains(NormalizePath filePath)

    let IsHidden (filePath : string) : bool = 
        let startsWithDot = filePath[0] = '.' && not (IsDotDir filePath)
        //let hasHiddenAttribute = f.Exists && (f.Attributes &&& FileAttributes.Hidden) <> 0
        startsWithDot

    let IsHiddenFile (f : FileSystemInfo) : bool = 
        (f.Name[0] = '.' && not (IsDotDir f.Name)) ||
        (f.Exists && (f.Attributes &&& FileAttributes.Hidden) = FileAttributes.Hidden)

    let SepCount (filePath : string) : int =
        filePath.ToCharArray()
        |> Array.filter (fun (c : char) -> c = Path.DirectorySeparatorChar)
        |> Array.length

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
