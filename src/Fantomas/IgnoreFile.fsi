namespace Fantomas

open System.IO.Abstractions
open Serilog

[<Struct>]
type AbsoluteFilePath =
    private
    | AbsoluteFilePath of string

    member Path: string
    static member Create: fs: IFileSystem -> filePath: string -> AbsoluteFilePath

/// The string argument is taken relative to the location
/// of the ignore-file.
type IsPathIgnored = AbsoluteFilePath -> bool

[<NoComparison; NoEquality>]
type IgnoreFile =
    {
        Location: IFileInfo
        IsIgnored: IsPathIgnored
    }

[<RequireQualifiedAccess>]
module IgnoreFile =

    [<Literal>]
    val IgnoreFileName: string = ".fantomasignore"

    /// Find the `.fantomasignore` file above the given filepath, if one exists.
    /// Note that this is intended for use only in the daemon; the command-line tool
    /// does not support `.fantomasignore` files anywhere other than the current
    /// working directory.
    val find: fs: IFileSystem -> loadIgnoreList: (string -> IsPathIgnored) -> filePath: string -> IgnoreFile option

    val loadIgnoreList: fs: IFileSystem -> ignoreFilePath: string -> IsPathIgnored

    /// The `.fantomasignore` the command line tool honours: the single one at or above the
    /// directory the tool was started from. The daemon instead finds the closest one to each
    /// file it is asked about.
    val findInDirectory:
        fs: IFileSystem -> currentDirectory: string -> loadIgnoreList: (string -> IsPathIgnored) -> IgnoreFile option

    /// Is the file matched by the ignore file? Deciding that is not something that should fail;
    /// if it does, the failure is reported through the sink and the file counts as not ignored.
    val isIgnoredFile: log: ILogger -> ignoreFile: IgnoreFile option -> file: string -> bool
