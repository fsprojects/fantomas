namespace Fantomas

open System.Collections.Concurrent
open System.IO.Abstractions
open Ignore
open Serilog

[<Struct>]
type AbsoluteFilePath =
    private
    | AbsoluteFilePath of string

    member x.Path =
        let (AbsoluteFilePath(path)) = x
        path

    static member Create (fs: IFileSystem) (filePath: string) =
        fs.Path.GetFullPath filePath |> AbsoluteFilePath

type IsPathIgnored = AbsoluteFilePath -> bool

[<Struct>]
type IgnoreMatch =
    {
        LineNumber: int
        Pattern: string
        Negated: bool
    }

[<NoComparison; NoEquality>]
type IgnoreFile =
    {
        Location: IFileInfo
        IsIgnored: IsPathIgnored
    }

[<RequireQualifiedAccess>]
module IgnoreFile =

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    let find (fs: IFileSystem) (loadIgnoreList: string -> IsPathIgnored) (filePath: string) : IgnoreFile option =
        let rec walkUp (currentDirectory: IDirectoryInfo) : IgnoreFile option =
            if isNull currentDirectory then
                None
            else

            let potentialFile =
                fs.Path.Combine(currentDirectory.FullName, IgnoreFileName) |> fs.FileInfo.New

            if not potentialFile.Exists then
                walkUp currentDirectory.Parent
            else

            {
                Location = potentialFile
                IsIgnored = loadIgnoreList potentialFile.FullName
            }
            |> Some

        walkUp (fs.FileInfo.New(filePath).Directory)

    // See https://git-scm.com/docs/gitignore
    // The incoming path is taken relative to the folder holding the ignore file.
    // In a cli scenario that is the current directory, for the daemon it is the first found ignore file.
    // .gitignore uses forward slashes to path separators
    //
    // The root is passed in rather than derived here, because the folder holding the ignore file
    // does not change between calls and looking it up again for every file walked meant a
    // directory lookup per file.
    let relativeToIgnoreRoot (fs: IFileSystem) (ignoreRoot: string) (absoluteFilePath: AbsoluteFilePath) : string =
        fs.Path.GetRelativePath(ignoreRoot, absoluteFilePath.Path).Replace("\\", "/")

    let loadIgnoreList (fs: IFileSystem) (ignoreFilePath: string) : IsPathIgnored =
        let lines: string array = fs.File.ReadAllLines(ignoreFilePath)

        let fantomasIgnore: Ignore =
            (Ignore(), lines)
            ||> Array.fold (fun (ig: Ignore) (line: string) -> ig.Add(line))

        let ignoreRoot: string = fs.Directory.GetParent(ignoreFilePath).FullName

        fun (absoluteFilePath: AbsoluteFilePath) ->
            fantomasIgnore.IsIgnored(relativeToIgnoreRoot fs ignoreRoot absoluteFilePath)

    let findInDirectory
        (fs: IFileSystem)
        (currentDirectory: string)
        (loadIgnoreList: string -> IsPathIgnored)
        : IgnoreFile option
        =
        // `find` walks up from a file, so it is given a name that need not exist in the directory.
        find fs loadIgnoreList (fs.Path.Combine(currentDirectory, "_"))

    let findAbove
        (fs: IFileSystem)
        (loadIgnoreList: string -> IsPathIgnored)
        (ignoreFile: IgnoreFile)
        : IgnoreFile list
        =
        // Starting one directory above the one holding it, so that the file itself is not the
        // first thing found again.
        let rec walkUp (currentDirectory: IDirectoryInfo) (found: IgnoreFile list) : IgnoreFile list =
            if isNull currentDirectory then
                List.rev found
            else

            let potentialFile: IFileInfo =
                fs.Path.Combine(currentDirectory.FullName, IgnoreFileName) |> fs.FileInfo.New

            let found: IgnoreFile list =
                if not potentialFile.Exists then
                    found
                else
                    {
                        Location = potentialFile
                        IsIgnored = loadIgnoreList potentialFile.FullName
                    }
                    :: found

            walkUp currentDirectory.Parent found

        // Every one above rather than the next one up. Two ignore files above the nearest is not a
        // layout anybody sets out to build, and it is exactly the layout where the reader has the
        // least chance of working out what happened on their own.
        walkUp ignoreFile.Location.Directory.Parent []

    let cachedFinder (fs: IFileSystem) (loadIgnoreList: string -> IsPathIgnored) : string -> IgnoreFile option =
        // Two caches, because a folder walk asks the same two questions over and over. Every file
        // in a directory resolves to the same ignore file, and every directory under one resolves
        // to that same file again, so without these a run over a thousand files walks the tree a
        // thousand times and compiles the same patterns as often.
        let byDirectory: ConcurrentDictionary<string, IgnoreFile option> =
            ConcurrentDictionary<string, IgnoreFile option>()

        let byIgnoreFile: ConcurrentDictionary<string, IsPathIgnored> =
            ConcurrentDictionary<string, IsPathIgnored>()

        let loadOnce (path: string) : IsPathIgnored =
            byIgnoreFile.GetOrAdd(path, System.Func<string, IsPathIgnored>(loadIgnoreList))

        fun (file: string) ->
            let directory: string =
                match fs.FileInfo.New(file).Directory with
                | null -> fs.Path.GetFullPath "."
                | directory -> directory.FullName

            byDirectory.GetOrAdd(directory, System.Func<string, IgnoreFile option>(fun _ -> find fs loadOnce file))

    let isIgnoredFile (log: ILogger) (ignoreFile: IgnoreFile option) (file: string) : bool =
        match ignoreFile with
        | None -> false
        | Some ignoreFile ->

        let fs: IFileSystem = ignoreFile.Location.FileSystem
        let fullPath: AbsoluteFilePath = AbsoluteFilePath.Create fs file

        try
            ignoreFile.IsIgnored fullPath
        with ex ->
            // Matching a path against the ignore file is not something that should fail. If it
            // does, say which file and which ignore file could not be told apart, and go on to
            // format the file rather than abandon the run over it.
            log.Error
                $"Could not tell whether '%s{file}' is matched by %s{ignoreFile.Location.FullName}: %s{ex.Message}"

            // The line above is the one to act on; this keeps the type and the stack trace for
            // whoever asks for detail.
            log.Debug $"%A{ex}"
            false

    let hasNegatedPattern (ignoreFile: IgnoreFile) : bool =
        let fs: IFileSystem = ignoreFile.Location.FileSystem

        fs.File.ReadAllLines ignoreFile.Location.FullName
        |> Array.exists (fun (line: string) ->
            // The library reads the line the way it reads it anywhere else, so a comment, a blank
            // line and a `\!` that means a literal exclamation mark are all told apart here the
            // same way they are told apart when the pattern is matched. A line it will not compile
            // is not a negation; whatever it is, it is not this function's to report.
            try
                IgnoreRule(line).Negate
            with _ ->
                false
        )

    let matchingLines (ignoreFile: IgnoreFile) (file: string) : IgnoreMatch list =
        let fs: IFileSystem = ignoreFile.Location.FileSystem
        let ignoreRoot: string = ignoreFile.Location.Directory.FullName

        let relativePath: string =
            relativeToIgnoreRoot fs ignoreRoot (AbsoluteFilePath.Create fs file)

        // One rule per line, rather than one `Ignore` holding all of them, which is the whole
        // difference between this and `IsIgnored`: the rules are asked separately so that the line
        // each answer came from is still known when the answers come back.
        fs.File.ReadAllLines ignoreFile.Location.FullName
        |> Array.toList
        |> List.indexed
        |> List.choose (fun (index: int, line: string) ->
            // A pattern the ignore library will not compile is not a match.
            //
            // Not the routine way a bad pattern is met: `loadIgnoreList` compiles every rule as it
            // reads the file, so an ignore file with one in it fails as a whole before any
            // `IgnoreFile` exists to ask this of. What is left for this to survive is the file
            // changing between that read and this one, which is a race rather than a mistake, and
            // is not worth abandoning the answer over.
            let matched: bool option =
                try
                    let rule: IgnoreRule = IgnoreRule(line)
                    if rule.IsMatch relativePath then Some rule.Negate else None
                with _ ->
                    None

            matched
            |> Option.map (fun (negated: bool) ->
                {
                    LineNumber = index + 1
                    Pattern = line
                    Negated = negated
                }
            )
        )
