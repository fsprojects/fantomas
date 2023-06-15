namespace Fantomas

open System.IO.Abstractions
open Ignore
open Fantomas.Logging

type AbsoluteFilePath =
    private
    | AbsoluteFilePath of string

    member x.Path =
        let (AbsoluteFilePath(path)) = x
        path

    static member Create (fs: IFileSystem) (filePath: string) =
        fs.Path.GetFullPath filePath |> AbsoluteFilePath

/// The string argument is taken relative to the location
/// of the ignore-file.
type IsPathIgnored = AbsoluteFilePath -> bool

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
                    fs.Path.Combine(currentDirectory.FullName, IgnoreFileName)
                    |> fs.FileInfo.FromFileName

                if potentialFile.Exists then
                    { Location = potentialFile
                      IsIgnored = loadIgnoreList potentialFile.FullName }
                    |> Some
                else
                    walkUp currentDirectory.Parent

        walkUp (fs.FileInfo.FromFileName(filePath).Directory)

    let loadIgnoreList (fs: IFileSystem) (ignoreFilePath: string) : IsPathIgnored =
        let lines = fs.File.ReadAllLines(ignoreFilePath)

        let fantomasIgnore =
            (Ignore(), lines)
            ||> Array.fold (fun (ig: Ignore) (line: string) -> ig.Add(line))

        fun (absoluteFilePath: AbsoluteFilePath) ->
            // See https://git-scm.com/docs/gitignore
            // We transform the incoming path relative to the .ignoreFilePath folder.
            // In a cli scenario that is the current directory, for the daemon it is the first found ignore file.
            // .gitignore uses forward slashes to path separators
            let relativePath =
                fs.Path
                    .GetRelativePath(fs.Directory.GetParent(ignoreFilePath).FullName, absoluteFilePath.Path)
                    .Replace("\\", "/")

            fantomasIgnore.IsIgnored(relativePath)

    let internal current' (fs: IFileSystem) (currentDirectory: string) (loadIgnoreList: string -> IsPathIgnored) =
        lazy find fs loadIgnoreList (fs.Path.Combine(currentDirectory, "_"))

    let current: Lazy<IgnoreFile option> =
        let fs = FileSystem()
        current' fs System.Environment.CurrentDirectory (loadIgnoreList fs)

    let isIgnoredAux (ignoreFile: IgnoreFile) (fullName: string) =
        let fs = ignoreFile.Location.FileSystem
        let fullPath = AbsoluteFilePath.Create fs fullName

        try
            ignoreFile.IsIgnored fullPath
        with ex ->
            elog $"%A{ex}"
            false

    let isIgnoredFile (ignoreFile: IgnoreFile option) (file: IFileInfo) : bool =
        match ignoreFile with
        | None -> false
        | Some ignoreFile -> isIgnoredAux ignoreFile file.FullName

    let isIgnoredFolder (ignoreFile: IgnoreFile option) (folder: IDirectoryInfo) : bool =
        match ignoreFile with
        | None -> false
        | Some ignoreFile -> isIgnoredAux ignoreFile folder.FullName
