namespace FsFindLib

open System

module FileResult = 

    type t = {
        Containers : string list;
        FilePath : string;
        Type: FileType
        Size : int64
        LastMod : DateTime Option
    }

    let Create (filePath : string) (fileType : FileType) (size : int64) (lastMod : DateTime Option) : t =
        {
            Containers=[];
            FilePath=filePath;
            Type=fileType;
            Size=size;
            LastMod=lastMod
        }

    let ToString (sf : t) : string =
        let container_str = 
            match sf.Containers with
            | [] -> ""
            | _  -> sprintf "%s!" (String.concat "!" sf.Containers)
        $"%s{container_str}%s{sf.FilePath}"
;;
