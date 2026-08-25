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

// Folders whose contents a compiler or a package manager wrote. Matched by name rather than by
// looking for the name inside a path, which is what a walk that descends a directory at a time
// makes possible, and which `objects` no longer has to be told apart from `obj`.
let excludedDirNames: HashSet<string> =
    HashSet<string>([| "obj"; ".fable"; "fable_modules"; "node_modules" |], StringComparer.Ordinal)

let isExcludedDirName (name: string) : bool = excludedDirNames.Contains name

let isFSharpFile (s: string) : bool =
    extensionLookup.Contains(Path.GetExtension s)

let findAllFilesRecursively (fs: IFileSystem) (isIgnoredDirectory: string -> bool) (path: string) : string seq =
    // A directory at a time rather than one flat enumeration, so that a folder nobody is going to
    // format is never opened. Asking about every file underneath an ignored folder and discarding
    // each answer is work, and it is also how a report came to say how many files are in a folder
    // it was told to stay out of.
    //
    // A stack rather than recursion. Written as a recursive `seq` it reads better, but `yield!`
    // there is not a tail call and cannot be made one: the sequence builder composes an enumerator
    // for every level, so a nested `yield!` pays for the depth of the tree on each element it
    // hands back. The loop below has neither the enumerator chain nor the stack frames, and the
    // order files come out in was never promised anyway, since the file system decides what
    // `GetDirectories` returns first.
    seq {
        let pending: Stack<string> = Stack<string>()
        pending.Push path

        while pending.Count > 0 do
            let directory: string = pending.Pop()

            yield! fs.Directory.GetFiles directory |> Seq.filter isFSharpFile

            for subdirectory in fs.Directory.GetDirectories directory do
                let name: string =
                    fs.Path.GetFileName(subdirectory.TrimEnd(Path.DirectorySeparatorChar))

                if not (isExcludedDirName name) && not (isIgnoredDirectory subdirectory) then
                    pending.Push subdirectory
    }

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
