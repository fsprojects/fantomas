namespace Fantomas

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

[<NoComparison; NoEquality>]
type IgnoreFile =
    { Location: IFileInfo
      IsIgnored: IsPathIgnored }

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

                if potentialFile.Exists then
                    { Location = potentialFile
                      IsIgnored = loadIgnoreList potentialFile.FullName }
                    |> Some
                else
                    walkUp currentDirectory.Parent

        walkUp (fs.FileInfo.New(filePath).Directory)

    let loadIgnoreList (fs: IFileSystem) (ignoreFilePath: string) : IsPathIgnored =
        let lines: string array = fs.File.ReadAllLines(ignoreFilePath)

        let fantomasIgnore: Ignore =
            (Ignore(), lines)
            ||> Array.fold (fun (ig: Ignore) (line: string) -> ig.Add(line))

        // The folder holding the ignore file does not change between calls, and looking it up
        // again for every file walked meant a directory lookup per file.
        let ignoreRoot: string = fs.Directory.GetParent(ignoreFilePath).FullName

        fun (absoluteFilePath: AbsoluteFilePath) ->
            // See https://git-scm.com/docs/gitignore
            // We transform the incoming path relative to the .ignoreFilePath folder.
            // In a cli scenario that is the current directory, for the daemon it is the first found ignore file.
            // .gitignore uses forward slashes to path separators
            let relativePath: string =
                fs.Path.GetRelativePath(ignoreRoot, absoluteFilePath.Path).Replace("\\", "/")

            fantomasIgnore.IsIgnored(relativePath)

    let findInDirectory
        (fs: IFileSystem)
        (currentDirectory: string)
        (loadIgnoreList: string -> IsPathIgnored)
        : IgnoreFile option =
        // `find` walks up from a file, so it is given a name that need not exist in the directory.
        find fs loadIgnoreList (fs.Path.Combine(currentDirectory, "_"))

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
