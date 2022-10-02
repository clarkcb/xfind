namespace FsFind

open System
open System.Collections.Generic
open System.IO
open System.Text.Json

type FileType = 
    | Unknown = 0
    | Archive = 1
    | Binary  = 2
    | Code    = 3
    | Text    = 4
    | Xml     = 5

type FileTypesDictionary = Dictionary<string, List<Dictionary<string,Object>>>

type FileTypes() =
    static let archive = "archive"
    static let binary = "binary"
    static let code = "code"
    static let text = "text"
    static let xml = "xml"

    let PopulateFileTypesFromJson (jsonString : string) =
        let fileTypeExtDictionary = Dictionary<string, ISet<string>>()
        let fileTypeNameDictionary = Dictionary<string, ISet<string>>()
        let filetypesDict = JsonSerializer.Deserialize<FileTypesDictionary>(jsonString)
        let filetypeDicts = filetypesDict.["filetypes"]
        for filetypeDict in filetypeDicts do
            let typeName = (filetypeDict.["type"] :?> JsonElement).GetString()
            let extensions =
                [ for x in (filetypeDict.["extensions"] :?> JsonElement).EnumerateArray() do
                    yield "." + x.GetString() ]
            fileTypeExtDictionary.Add(typeName, HashSet<String>(extensions))
            let names =
                [ for x in (filetypeDict.["names"] :?> JsonElement).EnumerateArray() do
                    yield x.GetString() ]
            fileTypeNameDictionary.Add(typeName, HashSet<String>(names))
        let allTextExts = HashSet<String>(fileTypeExtDictionary.[text])
        allTextExts.UnionWith(fileTypeExtDictionary.[code])
        allTextExts.UnionWith(fileTypeExtDictionary.[xml])
        if fileTypeExtDictionary.Remove(text) then
            fileTypeExtDictionary.Add(text, allTextExts)
        let allTextNames = HashSet<String>(fileTypeNameDictionary.[text])
        allTextNames.UnionWith(fileTypeNameDictionary.[code])
        allTextNames.UnionWith(fileTypeNameDictionary.[xml])
        if fileTypeNameDictionary.Remove(text) then
            fileTypeNameDictionary.Add(text, allTextNames)
        (fileTypeExtDictionary, fileTypeNameDictionary)

    let _fileTypesResource = EmbeddedResource.GetResourceFileContents("FsFindLib.Resources.filetypes.json")
    let _fileTypeExtDictionary, _fileTypeNameDictionary = PopulateFileTypesFromJson(_fileTypesResource)

    // read-only member properties
    member this.FileTypeExtDictionary = _fileTypeExtDictionary
    member this.FileTypeNameDictionary = _fileTypeNameDictionary

    static member FromName (name : string) : FileType =
        let lname = name.ToLowerInvariant()
        if lname.Equals(text) then FileType.Text
        else if lname.Equals(binary) then FileType.Binary
        else if lname.Equals(archive) then FileType.Archive
        else if lname.Equals(code) then FileType.Code
        else if lname.Equals(xml) then FileType.Xml
        else FileType.Unknown

    static member ToName (fileType : FileType) : string =
        match fileType with
        | FileType.Archive -> "Archive"
        | FileType.Binary -> "Binary"
        | FileType.Code -> "Code"
        | FileType.Text -> "Text"
        | FileType.Xml -> "Xml"
        | _ -> "Unknown"

    member this.GetFileType (f : FileInfo) : FileType =
        if this.IsArchiveFile f then FileType.Archive
        else if this.IsBinaryFile f then FileType.Binary
        else if this.IsCodeFile f then FileType.Code
        else if this.IsXmlFile f then FileType.Xml
        else if this.IsTextFile f then FileType.Text
        else FileType.Unknown

    member this.IsArchiveFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary.[archive] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary.[archive]

    member this.IsBinaryFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary.[binary] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary.[binary]

    member this.IsCodeFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary.[code] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary.[code]

    member this.IsTextFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary.[text] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary.[text]

    member this.IsUnknownFile (f : FileInfo) : bool =
        (this.GetFileType f) = FileType.Unknown

    member this.IsXmlFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary.[xml] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary.[xml]
    ;;
