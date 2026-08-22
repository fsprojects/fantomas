module Fantomas.Paths

open System
open System.IO

let extensions = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]

let isInExcludedDir (fullPath: string) =
    set [| "obj"; ".fable"; "fable_modules"; "node_modules" |]
    |> Set.map (fun dir -> sprintf "%c%s%c" Path.DirectorySeparatorChar dir Path.DirectorySeparatorChar)
    |> Set.exists fullPath.Contains

let isFSharpFile (s: string) =
    Set.contains (Path.GetExtension s) extensions

let findAllFilesRecursively path =
    let searchOption = SearchOption.AllDirectories

    Directory.GetFiles(path, "*.*", searchOption)
    |> Seq.filter (fun f -> isFSharpFile f && not (isInExcludedDir f))

let ensureParentFolderExists (file: string) : unit =
    let folder = Path.GetDirectoryName(file)

    if not (String.IsNullOrEmpty folder) then
        Directory.CreateDirectory(folder) |> ignore

let isSamePath (left: string) (right: string) : bool =
    String.Equals(Path.GetFullPath left, Path.GetFullPath right, StringComparison.Ordinal)

let isInFolder (folder: string) (file: string) : bool =
    let folder =
        String.Concat(
            Path.GetFullPath(folder).TrimEnd(Path.DirectorySeparatorChar),
            string<char> Path.DirectorySeparatorChar
        )

    Path.GetFullPath(file).StartsWith(folder, StringComparison.Ordinal)
