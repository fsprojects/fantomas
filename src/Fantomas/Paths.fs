module Fantomas.Paths

open System
open System.IO
open System.IO.Abstractions

let extensions: Set<string> = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]

let isInExcludedDir (fullPath: string) : bool =
    set [| "obj"; ".fable"; "fable_modules"; "node_modules" |]
    |> Set.map (fun dir -> sprintf "%c%s%c" Path.DirectorySeparatorChar dir Path.DirectorySeparatorChar)
    |> Set.exists fullPath.Contains

let isFSharpFile (s: string) : bool =
    Set.contains (Path.GetExtension s) extensions

let findAllFilesRecursively (fs: IFileSystem) (path: string) : string seq =
    fs.Directory.GetFiles(path, "*.*", SearchOption.AllDirectories)
    |> Seq.filter (fun f -> isFSharpFile f && not (isInExcludedDir f))

let ensureParentFolderExists (fs: IFileSystem) (file: string) : unit =
    let folder: string = fs.Path.GetDirectoryName(file)

    if not (String.IsNullOrEmpty folder) then
        fs.Directory.CreateDirectory(folder) |> ignore

let isSamePath (fs: IFileSystem) (left: string) (right: string) : bool =
    String.Equals(fs.Path.GetFullPath left, fs.Path.GetFullPath right, StringComparison.Ordinal)

let isInFolder (fs: IFileSystem) (folder: string) (file: string) : bool =
    let folder: string =
        String.Concat(
            fs.Path.GetFullPath(folder).TrimEnd(fs.Path.DirectorySeparatorChar),
            string<char> fs.Path.DirectorySeparatorChar
        )

    fs.Path.GetFullPath(file).StartsWith(folder, StringComparison.Ordinal)
