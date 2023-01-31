namespace FsFind

type SortBy = 
    | FilePath = 0
    | FileName = 1
    | FileSize = 2
    | FileType = 3
    | LastMod  = 4


module SortUtil = 
    
    let SortByFromName (name : string) : SortBy =
        let uname = name.ToUpperInvariant()
        if uname.Equals("NAME") then SortBy.FileName
        else if uname.Equals("SIZE") then SortBy.FileSize
        else if uname.Equals("TYPE") then SortBy.FileType
        else if uname.Equals("LASTMOD") then SortBy.LastMod
        else SortBy.FilePath

    let NameFromSortBy (sortBy : SortBy) : string =
        if sortBy = SortBy.FileName then "NAME"
        else if sortBy = SortBy.FileSize then "SIZE"
        else if sortBy = SortBy.FileType then "TYPE"
        else if sortBy = SortBy.LastMod then "LASTMOD"
        else "PATH"

;;
