namespace FsFind

type SortBy = 
    | FilePath = 0
    | FileName = 1
    | FileType = 2


module SortUtil = 
    
    let SortByFromName (name : string) : SortBy =
        let lname = name.ToUpperInvariant()
        if lname.Equals("NAME") then SortBy.FileName
        else if lname.Equals("TYPE") then SortBy.FileType
        else SortBy.FilePath

    let NameFromSortBy (sortBy : SortBy) : string =
        if sortBy = SortBy.FileName then "NAME"
        else if sortBy = SortBy.FileType then "TYPE"
        else "PATH"


;;
