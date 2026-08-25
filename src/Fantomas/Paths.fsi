module Fantomas.Paths

open System.IO.Abstractions

/// The file extensions Fantomas formats.
val extensions: Set<string>

/// Is this the name of a folder whose contents a compiler or a package manager wrote?
val isExcludedDirName: name: string -> bool

val isFSharpFile: s: string -> bool

/// Every F# file below the given path, at any depth.
///
/// Two kinds of folder are not descended into. Build output and package folders, because
/// formatting what a compiler or a package manager wrote is never what was asked for. And any
/// folder `isIgnoredDirectory` answers for, which is how `.fantomasignore` naming a folder is
/// honoured: not by asking about every file inside it and discarding each answer, but by never
/// opening it. A run should have no more idea what is in a folder it was told to stay out of than
/// it has about a folder that is not there.
val findAllFilesRecursively: fs: IFileSystem -> isIgnoredDirectory: (string -> bool) -> path: string -> string seq

/// Create the folders leading up to a file, so that writing to a path the user named but never
/// created succeeds. GetDirectoryName yields an empty string for a bare file name.
val ensureParentFolderExists: fs: IFileSystem -> file: string -> unit

/// Do two paths name the same location? `src` and `./src` do, and comparing them as they were
/// typed does not say so. This is about spelling, not about the file system: a path reached
/// through a symbolic link, or through a spelling a case insensitive volume accepts, is not
/// recognised here. Nothing may depend on a negative answer to avoid destroying a file.
val isSamePath: fs: IFileSystem -> left: string -> right: string -> bool

/// Is a file located inside a folder, at any depth?
val isInFolder: fs: IFileSystem -> folder: string -> file: string -> bool
