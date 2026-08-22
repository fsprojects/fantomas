module Fantomas.Paths

open System
open System.IO
open System.IO.Abstractions

let extensions: Set<string> = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]

let isInExcludedDir (fullPath: string) : bool =
    set [| "obj"; ".fable"; "fable_modules"; "node_modules" |]
    |> Set.map (fun dir -> sprintf "%c%s%c" Path.DirectorySeparatorChar dir Path.DirectorySeparatorChar)
    |> Set.exists fullPath.Contains

// The extension is compared without case, so that a file named A.FS is the F# file it plainly is.
// On a volume that ignores case, which is the usual one on macOS and Windows, rejecting it would
// mean refusing to format a file the compiler happily builds.
let isFSharpFile (s: string) : bool =
    Set.contains (Path.GetExtension(s).ToLowerInvariant()) extensions

let findAllFilesRecursively (fs: IFileSystem) (path: string) : string seq =
    fs.Directory.GetFiles(path, "*.*", SearchOption.AllDirectories)
    |> Seq.filter (fun f -> isFSharpFile f && not (isInExcludedDir f))

let ensureParentFolderExists (fs: IFileSystem) (file: string) : unit =
    let folder: string = fs.Path.GetDirectoryName(file)

    if not (String.IsNullOrEmpty folder) then
        fs.Directory.CreateDirectory(folder) |> ignore

let isSamePath (fs: IFileSystem) (left: string) (right: string) : bool =
    // A trailing separator names the same place, so `src` and `src/` have to compare equal. They
    // do not otherwise, and a folder formatted to itself under the longer spelling would be taken
    // for a previous run's output and skipped entirely.
    let resolve (path: string) : string =
        fs.Path.GetFullPath(path).TrimEnd(fs.Path.DirectorySeparatorChar, fs.Path.AltDirectorySeparatorChar)

    String.Equals(resolve left, resolve right, StringComparison.Ordinal)

let isInFolder (fs: IFileSystem) (folder: string) (file: string) : bool =
    let folder: string =
        String.Concat(
            fs.Path.GetFullPath(folder).TrimEnd(fs.Path.DirectorySeparatorChar),
            string<char> fs.Path.DirectorySeparatorChar
        )

    fs.Path.GetFullPath(file).StartsWith(folder, StringComparison.Ordinal)
