namespace FsFindLib

open System
open System.Collections.Generic
open System.IO
open System.Text.Json

type FileType = 
    | Unknown = 0
    | Archive = 1
    | Audio   = 2
    | Binary  = 3
    | Code    = 4
    | Font    = 5
    | Image   = 6
    | Text    = 7
    | Video   = 8
    | Xml     = 9

type FileTypesDictionary = Dictionary<string, List<Dictionary<string,Object>>>

type FileTypes() =
    static let archive = "archive"
    static let audio = "audio"
    static let binary = "binary"
    static let code = "code"
    static let font = "font"
    static let image = "image"
    static let text = "text"
    static let video = "video"
    static let xml = "xml"
    static let unknown = "unknown"

    let PopulateFileTypesFromJson (jsonString : string) =
        let fileTypeExtDictionary = Dictionary<string, ISet<string>>()
        let fileTypeNameDictionary = Dictionary<string, ISet<string>>()
        let filetypesDict = JsonSerializer.Deserialize<FileTypesDictionary>(jsonString)
        let filetypeDicts = filetypesDict["filetypes"]
        for filetypeDict in filetypeDicts do
            let typeName = (filetypeDict["type"] :?> JsonElement).GetString()
            let extensions =
                [ for x in (filetypeDict["extensions"] :?> JsonElement).EnumerateArray() do
                    yield "." + x.GetString() ]
            fileTypeExtDictionary.Add(typeName, HashSet<String>(extensions))
            let names =
                [ for x in (filetypeDict["names"] :?> JsonElement).EnumerateArray() do
                    yield x.GetString() ]
            fileTypeNameDictionary.Add(typeName, HashSet<String>(names))
        let allTextExts = HashSet<String>(fileTypeExtDictionary[text])
        allTextExts.UnionWith(fileTypeExtDictionary[code])
        allTextExts.UnionWith(fileTypeExtDictionary[xml])
        if fileTypeExtDictionary.Remove(text) then
            fileTypeExtDictionary.Add(text, allTextExts)
        let allTextNames = HashSet<String>(fileTypeNameDictionary[text])
        allTextNames.UnionWith(fileTypeNameDictionary[code])
        allTextNames.UnionWith(fileTypeNameDictionary[xml])
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
        if lname.Equals(archive) then FileType.Archive
        else if lname.Equals(audio) then FileType.Audio
        else if lname.Equals(binary) then FileType.Binary
        else if lname.Equals(code) then FileType.Code
        else if lname.Equals(font) then FileType.Font
        else if lname.Equals(image) then FileType.Image
        else if lname.Equals(text) then FileType.Text
        else if lname.Equals(video) then FileType.Video
        else if lname.Equals(xml) then FileType.Xml
        else FileType.Unknown

    static member ToName (fileType : FileType) : string =
        match fileType with
        | FileType.Archive -> archive
        | FileType.Audio -> audio
        | FileType.Binary -> binary
        | FileType.Code -> code
        | FileType.Font -> font
        | FileType.Image -> image
        | FileType.Text -> text
        | FileType.Video -> video
        | FileType.Xml -> xml
        | _ -> unknown

    member this.GetFileType (f : FileInfo) : FileType =
        // most specific first
        if this.IsCodeFile f then FileType.Code
        else if this.IsArchiveFile f then FileType.Archive
        else if this.IsAudioFile f then FileType.Audio
        else if this.IsFontFile f then FileType.Font
        else if this.IsImageFile f then FileType.Image
        else if this.IsVideoFile f then FileType.Video
        // most general last
        else if this.IsXmlFile f then FileType.Xml
        else if this.IsTextFile f then FileType.Text
        else if this.IsBinaryFile f then FileType.Binary
        else FileType.Unknown

    member this.IsArchiveFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[archive] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[archive]

    member this.IsAudioFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[audio] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[audio]

    member this.IsBinaryFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[binary] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[binary]

    member this.IsCodeFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[code] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[code]

    member this.IsFontFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[font] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[font]

    member this.IsImageFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[image] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[image]

    member this.IsTextFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[text] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[text]

    member this.IsVideoFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[video] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[video]

    member this.IsUnknownFile (f : FileInfo) : bool =
        (this.GetFileType f) = FileType.Unknown

    member this.IsXmlFile (f : FileInfo) : bool =
        Seq.exists (fun x -> x = f.Name) this.FileTypeNameDictionary[xml] ||
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypeExtDictionary[xml]
    ;;
