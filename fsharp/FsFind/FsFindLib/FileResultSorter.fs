namespace FsFindLib

open System

type FileResultSorter (settings : FindSettings) =

    member this.CompareByPath (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let cmp = if settings.SortCaseInsensitive then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
        let dirNameCmp = String.Compare(fr1.File.DirectoryName, fr2.File.DirectoryName, cmp)
        if dirNameCmp = 0 then String.Compare(fr1.File.Name, fr2.File.Name, cmp) else dirNameCmp

    member this.CompareByName (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let cmp = if settings.SortCaseInsensitive then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
        let fileNameCmp = String.Compare(fr1.File.Name, fr2.File.Name, cmp)
        if fileNameCmp = 0 then String.Compare(fr1.File.DirectoryName, fr2.File.DirectoryName, cmp) else fileNameCmp

    member this.CompareBySize (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let sizeCmp = fr1.File.Length.CompareTo(fr2.File.Length)
        if sizeCmp = 0 then (this.CompareByPath fr1 fr2) else sizeCmp

    member this.CompareByType (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let typeCmp = fr1.FileType.CompareTo(fr2.FileType)
        if typeCmp = 0 then (this.CompareByPath fr1 fr2) else typeCmp

    member this.CompareByLastMod (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let lastModCmp = fr1.File.LastWriteTimeUtc.CompareTo(fr2.File.LastWriteTimeUtc)
        if lastModCmp = 0 then (this.CompareByPath fr1 fr2) else lastModCmp

    member this.GetFileResultComparator : FileResult.t -> FileResult.t -> int =
        if settings.SortDescending then
            match settings.SortBy with
            | SortBy.FileName -> (fun fr1 fr2 -> this.CompareByName fr2 fr1)
            | SortBy.FileSize -> (fun fr1 fr2 -> this.CompareBySize fr2 fr1)
            | SortBy.FileType -> (fun fr1 fr2 -> this.CompareByType fr2 fr1)
            | SortBy.LastMod  -> (fun fr1 fr2 -> this.CompareByLastMod fr2 fr1)
            | _               -> (fun fr1 fr2 -> this.CompareByPath fr2 fr1)
        else
            match settings.SortBy with
            | SortBy.FileName -> this.CompareByName
            | SortBy.FileSize -> this.CompareBySize
            | SortBy.FileType -> this.CompareByType
            | SortBy.LastMod  -> this.CompareByLastMod
            | _               -> this.CompareByPath

    member this.Sort (fileResults : FileResult.t list) : FileResult.t list =
        let fileResultComparator = this.GetFileResultComparator
        List.sortWith fileResultComparator fileResults

;;
