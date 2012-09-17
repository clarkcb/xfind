﻿namespace FsSearch

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Xml.Linq

type FileUtil() =
    let _fileTypesPath = new FileInfo(@"Z:\cary\src\git\xsearch\shared\filetypes.xml")
    let _fileTypesDictionary = Utils.PopulateFileTypes(_fileTypesPath)

    // read-only member properties
    member this.FileTypesPath = _fileTypesPath
    member this.FileTypesDictionary = _fileTypesDictionary

    member this.IsBinaryFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.["binary"]

    member this.IsCompressedFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.["compressed"]

    member this.IsSearchableFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.["searchable"]

    member this.IsTextFile (f : FileInfo) =
        Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) this.FileTypesDictionary.["text"]

    member this.IsUnknownFile (f : FileInfo) =
        (Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _fileTypesDictionary.["unknown"]) ||
        (not (Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _fileTypesDictionary.["searchable"]) &&
         not (Seq.exists (fun x -> x = f.Extension.ToLowerInvariant()) _fileTypesDictionary.["nosearch"]))

    ;;
