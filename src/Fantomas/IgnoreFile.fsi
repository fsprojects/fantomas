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

    /// Find the `.fantomasignore` file above the given filepath, if one exists: the nearest one at
    /// or above it, and only that one, so a file in a subfolder is governed by the ignore file
    /// beside it rather than by one further up.
    ///
    /// Note that this is the nearest and not the union of every one above, which is where it
    /// differs from `.gitignore`.
    val find: fs: IFileSystem -> loadIgnoreList: (string -> IsPathIgnored) -> filePath: string -> IgnoreFile option

    val loadIgnoreList: fs: IFileSystem -> ignoreFilePath: string -> IsPathIgnored

    /// The single `.fantomasignore` at or above a directory. Kept for a caller that has a directory
    /// rather than a file; a run over files wants `cachedFinder`.
    val findInDirectory:
        fs: IFileSystem -> currentDirectory: string -> loadIgnoreList: (string -> IsPathIgnored) -> IgnoreFile option

    /// `find`, per file, remembering what it found.
    ///
    /// This is what the command line uses, so that it and the daemon answer the same way about the
    /// same file. They used not to: the daemon resolved the nearest ignore file to each file it was
    /// asked about, while the command line resolved one for the whole run from the directory it was
    /// started in, so a `.fantomasignore` in a subfolder was honoured by an editor and invisible to
    /// a pipeline. The same file was skipped in one and formatted in the other.
    ///
    /// Cached because a folder walk asks about every file in turn, and without it the tree is
    /// walked and the patterns compiled once per file rather than once per directory.
    val cachedFinder: fs: IFileSystem -> loadIgnoreList: (string -> IsPathIgnored) -> (string -> IgnoreFile option)

    /// Is the file matched by the ignore file? Deciding that is not something that should fail;
    /// if it does, the failure is reported through the sink and the file counts as not ignored.
    val isIgnoredFile: log: ILogger -> ignoreFile: IgnoreFile option -> file: string -> bool
