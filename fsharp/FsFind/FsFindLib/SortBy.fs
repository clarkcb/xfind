namespace FsFind

type SortBy = 
    | FilePath = 0
    | FileName = 1
    | FileSize = 2
    | FileType = 3
    | LastMod  = 4


module SortUtil = 
    
    let SortByFromName (name : string) : SortBy =
        let lname = name.ToLowerInvariant()
        if lname.Equals("filename") || lname.Equals("name") then SortBy.FileName
        else if lname.Equals("filesize") || lname.Equals("size") then SortBy.FileSize
        else if lname.Equals("filetype") || lname.Equals("type") then SortBy.FileType
        else if lname.Equals("lastmod") then SortBy.LastMod
        else SortBy.FilePath

    let NameFromSortBy (sortBy : SortBy) : string =
        if sortBy = SortBy.FileName then "filename"
        else if sortBy = SortBy.FileSize then "filesize"
        else if sortBy = SortBy.FileType then "filetype"
        else if sortBy = SortBy.LastMod then "lastmod"
        else "filepath"

;;
