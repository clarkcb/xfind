namespace FsFindLib

open System
open System.IO

type FileResultSorter (settings : FindSettings) =

    member this.CompareByPath (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let cmp = if settings.SortCaseInsensitive then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
        let dirPathCmp = String.Compare(Path.GetDirectoryName(fr1.FilePath), Path.GetDirectoryName(fr2.FilePath), cmp)
        if dirPathCmp = 0 then String.Compare(Path.GetFileName(fr1.FilePath), Path.GetFileName(fr2.FilePath), cmp) else dirPathCmp

    member this.CompareByName (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let cmp = if settings.SortCaseInsensitive then StringComparison.OrdinalIgnoreCase else StringComparison.Ordinal
        let fileNameCmp = String.Compare(Path.GetFileName(fr1.FilePath), Path.GetFileName(fr2.FilePath), cmp)
        if fileNameCmp = 0 then String.Compare(Path.GetDirectoryName(fr1.FilePath), Path.GetDirectoryName(fr2.FilePath), cmp) else fileNameCmp

    member this.CompareBySize (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let sizeCmp = fr1.Size.CompareTo(fr2.Size)
        if sizeCmp = 0 then (this.CompareByPath fr1 fr2) else sizeCmp

    member this.CompareByType (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let typeCmp = fr1.Type.CompareTo(fr2.Type)
        if typeCmp = 0 then (this.CompareByPath fr1 fr2) else typeCmp

    member this.CompareByLastMod (fr1 : FileResult.t) (fr2 : FileResult.t) : int =
        let lastModCmp = fr1.LastMod.Value.CompareTo(fr2.LastMod.Value)
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
