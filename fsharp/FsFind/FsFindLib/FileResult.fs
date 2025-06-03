namespace FsFindLib

open System.IO

module FileResult = 

    type t = {
        Containers : string list;
        File : FileInfo;
        FileType : FileType;
    }

    let Create (file : FileInfo) (fileType : FileType) : t =
        {
            Containers=[];
            File=file;
            FileType=fileType
        }

    let ToString (sf : t) : string =
        let container_str = 
            match sf.Containers with
            | [] -> ""
            | _  -> sprintf "%s!" (String.concat "!" sf.Containers)
        $"%s{container_str}%s{sf.File.ToString()}"
;;
