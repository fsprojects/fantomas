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

/// One line of a `.fantomasignore` whose pattern matches a path.
[<Struct>]
type IgnoreMatch =
    {
        /// Counting from one, so it can be quoted the way an editor numbers the file and the way
        /// `git check-ignore -v` reports the pattern that decided.
        LineNumber: int
        /// The line exactly as it is written, `!` and all.
        Pattern: string
        /// Whether the pattern begins with `!`, which takes a path back out of what a pattern
        /// above it matched.
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

    /// Every line of the ignore file whose pattern matches the path, in the order they are
    /// written. The last of them is the one that decided: a pattern overrules every pattern above
    /// it, so a `!` line that comes last un-ignores what an earlier line matched and an ordinary
    /// line that comes last ignores what an earlier `!` line let through.
    ///
    /// `IsIgnored` answers yes or no, which is what a run needs and is not what somebody staring
    /// at an ignore file somebody else wrote needs. This is the same question asked one pattern at
    /// a time, so that the answer can be quoted back with the line that gave it.
    ///
    /// Empty and blank lines and lines beginning with `#` match nothing, so they never appear
    /// here. Neither does a pattern the ignore library will not compile: that is a fault in the
    /// ignore file rather than a match, and it is not this function's to report.
    val matchingLines: ignoreFile: IgnoreFile -> file: string -> IgnoreMatch list
