module Fantomas.InputFiles

open System.IO.Abstractions

/// .fs, .fsx, .fsi, .ml, .mli
val acceptedFSharpExtensions: string Set

/// Get all input files recursively for the given folder.
/// The files are checked against the accepted extensions, excluded folders and `.fantomasignore`
/// Meant to be used in the command line tool.
val getFilesForFolder: fantomasIgnoreFile: IgnoreFile option -> directory: IDirectoryInfo -> seq<IFileInfo>

/// Verify the file extension is part of the `acceptedFSharpExtensions`
val isFSharpFile: file: IFileInfo -> bool
