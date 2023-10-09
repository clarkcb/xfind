namespace FsFind

open System.IO

module FileResult = 

    type t = {
        Containers : string list;
        File : FileInfo;
        FileType : FileType;
        MimeType : string;
    }

    let Create (file : FileInfo) (fileType : FileType) (mimeType : string) : t =
        {
            Containers=[];
            File=file;
            FileType=fileType;
            MimeType=mimeType
        }

    let ToString (fr : t) : string =
        let container_str = 
            match fr.Containers with
            | [] -> ""
            | _  -> sprintf "%s!" (String.concat "!" fr.Containers)
        let mimetype_str = 
            match fr.MimeType with
            | "" -> ""
            | _  -> $" (%s{fr.MimeType})"
        $"%s{container_str}%s{fr.File.ToString()}%s{mimetype_str}"
;;
