module Fantomas.Paths

open System
open System.Collections.Generic
open System.IO
open System.IO.Abstractions

let extensions: Set<string> = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]

// The extension is compared without case, so that a file named A.FS is the F# file it plainly is.
// On a volume that ignores case, which is the usual one on macOS and Windows, rejecting it would
// mean refusing to format a file the compiler happily builds. A set that ignores case does that
// without lowering a copy of every extension it is asked about; this runs once per file walked.
let extensionLookup: HashSet<string> =
    HashSet<string>(extensions, StringComparer.OrdinalIgnoreCase)

// The names to look for, with a separator either side so that `objects` is not read as `obj`.
// Worked out once: building them per file made walking a folder several times slower than it
// needs to be. Both separators are covered, since a path can reach us spelled either way.
let excludedDirFragments: string array =
    let separators: char array =
        if Path.DirectorySeparatorChar = Path.AltDirectorySeparatorChar then
            [| Path.DirectorySeparatorChar |]
        else
            [| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]

    [| for dir in [| "obj"; ".fable"; "fable_modules"; "node_modules" |] do
           for separator in separators -> String.Concat(string<char> separator, dir, string<char> separator) |]

let isInExcludedDir (fullPath: string) : bool =
    excludedDirFragments
    |> Array.exists (fun (fragment: string) -> fullPath.Contains(fragment, StringComparison.Ordinal))

let isFSharpFile (s: string) : bool =
    extensionLookup.Contains(Path.GetExtension s)

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
