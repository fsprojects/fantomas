namespace Fantomas

open System.IO.Abstractions

type AbsoluteFilePath =
    private
    | AbsoluteFilePath of string

    member Path: string
    static member Create: fs: IFileSystem -> filePath: string -> AbsoluteFilePath

/// The string argument is taken relative to the location
/// of the ignore-file.
type IsPathIgnored = AbsoluteFilePath -> bool

type IgnoreFile =
    { Location: IFileInfo
      IsIgnored: IsPathIgnored }

module IgnoreFile =

    [<Literal>]
    val IgnoreFileName: string = ".fantomasignore"

    /// Find the `.fantomasignore` file above the given filepath, if one exists.
    /// Note that this is intended for use only in the daemon; the command-line tool
    /// does not support `.fantomasignore` files anywhere other than the current
    /// working directory.
    val find: fs: IFileSystem -> loadIgnoreList: (string -> IsPathIgnored) -> filePath: string -> IgnoreFile option

    val loadIgnoreList: fs: IFileSystem -> ignoreFilePath: string -> IsPathIgnored

    val internal current':
        fs: IFileSystem ->
        currentDirectory: string ->
        loadIgnoreList: (string -> IsPathIgnored) ->
            Lazy<IgnoreFile option>

    /// When executed from the command line, Fantomas will not dynamically locate
    /// the most appropriate `.fantomasignore` for each input file; it only finds
    /// a single `.fantomasignore` file. This is that file.
    val current: Lazy<IgnoreFile option>

    val isIgnoredFile: ignoreFile: IgnoreFile option -> file: string -> bool
