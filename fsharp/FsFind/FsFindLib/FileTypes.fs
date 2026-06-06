namespace FsFindLib

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
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

    member this.GetFileTypeForFilePath (filePath: string) : FileType =
        // most specific first
        if this.IsCodeFilePath filePath then FileType.Code
        else if this.IsArchiveFilePath filePath then FileType.Archive
        else if this.IsAudioFilePath filePath then FileType.Audio
        else if this.IsFontFilePath filePath then FileType.Font
        else if this.IsImageFilePath filePath then FileType.Image
        else if this.IsVideoFilePath filePath then FileType.Video
        // most general last
        else if this.IsXmlFilePath filePath then FileType.Xml
        else if this.IsTextFilePath filePath then FileType.Text
        else if this.IsBinaryFilePath filePath then FileType.Binary
        else FileType.Unknown

    member this.IsFilePathForType (filePath: string, typeName : string) : bool =
        let fileName = Path.GetFileName(filePath)
        let ext = FileUtil.GetFilePathExtension(filePath)
        Seq.exists (fun x -> x = fileName) this.FileTypeNameDictionary[typeName] ||
        Seq.exists (fun x -> x = ext) this.FileTypeExtDictionary[typeName]

    member this.IsArchiveFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, archive)

    member this.IsAudioFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, audio)

    member this.IsBinaryFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, binary)

    member this.IsCodeFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, code)

    member this.IsFontFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, font)

    member this.IsImageFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, image)

    member this.IsTextFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, text)

    member this.IsVideoFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, video)

    member this.IsUnknownFilePath (filePath: string) : bool =
        (this.GetFileTypeForFilePath filePath) = FileType.Unknown

    member this.IsXmlFilePath (filePath: string) : bool =
        this.IsFilePathForType(filePath, xml)

module FileTypesUtil =
    let FileTypesListToString (lst : FileType list) : string = 
        let rec recListToString (acc : string) (lst : FileType list) =
            match lst with
            | []     -> acc.Trim()
            | [a]    -> (recListToString (acc + " " + (FileTypes.ToName a)) [])
            | h :: t -> (recListToString (acc + " " + (FileTypes.ToName h) + ",") t) in
        sprintf "[%s]" (recListToString "" lst)

    let FileTypesListFromString (fts : string) : FileType list =
        let nonWord = Regex(@"\W+")
        nonWord.Split(fts)
        |> Array.toList
        |> List.filter (fun (x : string) -> String.IsNullOrEmpty(x) = false)
        |> List.map (fun (x : string) -> FileTypes.FromName x)
    ;;
