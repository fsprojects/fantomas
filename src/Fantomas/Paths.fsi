module Fantomas.Paths

/// The file extensions Fantomas formats.
val extensions: Set<string>

/// Is the path inside a folder whose contents a compiler or a package manager wrote?
val isInExcludedDir: fullPath: string -> bool

val isFSharpFile: s: string -> bool

/// Every F# file below the given path, at any depth. Build output and package folders are
/// skipped: formatting what a compiler or a package manager wrote is never what was asked for.
val findAllFilesRecursively: path: string -> string seq

/// Create the folders leading up to a file, so that writing to a path the user named but never
/// created succeeds. Path.GetDirectoryName yields an empty string for a bare file name.
val ensureParentFolderExists: file: string -> unit

/// Do two paths name the same location? `src` and `./src` do, and comparing them as they were
/// typed does not say so. This is about spelling, not about the file system: a path reached
/// through a symbolic link, or through a spelling a case insensitive volume accepts, is not
/// recognised here. Nothing may depend on a negative answer to avoid destroying a file.
val isSamePath: left: string -> right: string -> bool

/// Is a file located inside a folder, at any depth?
val isInFolder: folder: string -> file: string -> bool
