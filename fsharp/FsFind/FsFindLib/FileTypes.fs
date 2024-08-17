namespace FsFindLib

open Microsoft.Data.Sqlite
open System
open System.Collections.Generic
open System.Data
open System.IO

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
    
    let _conn = new SqliteConnection("Data Source=" + FindConfig.XfindDb + ";Mode=ReadOnly")

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

    member this.GetConnection () : SqliteConnection =
        if _conn.State = ConnectionState.Closed then _conn.Open()
        _conn
    
    member this.GetFileTypeForQueryAndElem (query : string) (elem : string) : FileType =
        let conn = this.GetConnection()
        use command = conn.CreateCommand()
        command.CommandText <- query
        let _ = command.Parameters.AddWithValue("$x0", elem)
        let reader : SqliteDataReader = command.ExecuteReader()
        if reader.Read() then
            enum<FileType>(reader.GetInt32(0) - 1)
        else
            FileType.Unknown

    member this.GetFileTypeForFileName (fileName : string) : FileType =
        if String.IsNullOrEmpty(fileName) then
            FileType.Unknown
        else
            let query = "SELECT file_type_id FROM file_name WHERE name = $x0"
            this.GetFileTypeForQueryAndElem query fileName

    member this.GetFileTypeForExtension (fileExt : string) : FileType =
        if String.IsNullOrEmpty(fileExt) then
            FileType.Unknown
        else
            let query = "SELECT file_type_id FROM file_extension WHERE extension = $x0"
            this.GetFileTypeForQueryAndElem query fileExt

    member this.GetFileType (fi : FileInfo) : FileType =
        match this.GetFileTypeForFileName(fi.Name) with
        | FileType.Unknown -> this.GetFileTypeForExtension(FileUtil.GetFileExtension(fi))
        | fileType -> fileType

    member this.IsArchiveFile (f : FileInfo) : bool =
        this.GetFileType(f) = FileType.Archive

    member this.IsAudioFile (f : FileInfo) : bool =
        this.GetFileType(f) = FileType.Audio

    member this.IsBinaryFile (f : FileInfo) : bool =
        this.GetFileType(f) = FileType.Binary

    member this.IsCodeFile (f : FileInfo) : bool =
        this.GetFileType(f) = FileType.Code

    member this.IsFontFile (f : FileInfo) : bool =
        this.GetFileType(f) = FileType.Font

    member this.IsImageFile (f : FileInfo) : bool =
        this.GetFileType(f) = FileType.Image

    member this.IsTextFile (f : FileInfo) : bool =
        let fileType = this.GetFileType(f)
        fileType = FileType.Text
        || fileType = FileType.Code
        || fileType = FileType.Xml

    member this.IsVideoFile (f : FileInfo) : bool =
        this.GetFileType(f) = FileType.Video

    member this.IsUnknownFile (f : FileInfo) : bool =
        (this.GetFileType f) = FileType.Unknown

    member this.IsXmlFile (f : FileInfo) : bool =
        this.GetFileType(f) = FileType.Xml
    ;;
